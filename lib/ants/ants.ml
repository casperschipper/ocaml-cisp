type sync = Modified | Synced | Pristine

type node = Node of {id: int; x: float; y: float; z: float; sync: sync}

(* --- JSON ENCODER/DECODER FOR node --- *)

let sync_to_yojson = function
  | Pristine -> `String "pristine"
  | Modified -> `String "modified"
  | Synced -> `String "synced"

let sync_of_yojson = function
  | `String "pristine" -> Ok Pristine
  | `String "modified" -> Ok Modified 
  | `String "synced" -> Ok Synced
  | _ -> Error "Unknown sync value"

let node_to_yojson (Node {id; x; y; z; sync}) =
  `Assoc
    [ ("id", `Int id)
    ; ("x", `Float x)
    ; ("y", `Float y)
    ; ("z", `Float z)
    ; ("sync", sync_to_yojson sync) ]

let node_of_yojson = function
  | `Assoc fields -> (
      try
        let id = match List.assoc "id" fields with `Int i -> i | _ -> failwith "id" in
        let x = match List.assoc "x" fields with `Float f -> f | _ -> failwith "x" in
        let y = match List.assoc "y" fields with `Float f -> f | _ -> failwith "y" in
        let z = match List.assoc "z" fields with `Float f -> f | _ -> failwith "z" in
        let sync = match List.assoc "sync" fields |> sync_of_yojson with Ok s -> s | Error _ -> failwith "sync" in
        Ok (Node {id; x; y; z; sync})
      with _ -> Error "node_of_yojson: invalid fields"
    )
  | _ -> Error "node_of_yojson: not an object"

(* --- SEQ FILE IO FOR node --- *)

let write_node_seq_to_file filename (nodes : node Seq.t) =
  let oc = open_out filename in
  output_char oc '[';
  let first = ref true in
  let rec loop s =
    match s () with
    | Seq.Nil -> ()
    | Seq.Cons (node, tl) ->
        if !first then first := false else output_char oc ',';
        Yojson.Safe.to_channel oc (node_to_yojson node);
        loop tl
  in
  loop nodes;
  output_char oc ']';
  close_out oc

let read_node_seq_from_file filename : node Seq.t =
  let ic = open_in filename in
  let json = Yojson.Safe.from_channel ic in
  close_in ic;
  match json with
  | `List lst ->
      let rec seq_of_list = function
        | [] -> Seq.Nil
        | hd :: tl ->
            (match node_of_yojson hd with
            | Ok node -> Seq.Cons (node, fun () -> seq_of_list tl)
            | Error _ -> seq_of_list tl)
      in
      fun () -> seq_of_list lst
  | _ -> fun () -> Seq.Nil