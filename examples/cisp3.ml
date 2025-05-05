open Cisp

let boundedwalk min max step =
  let open Seq in
  let update ((current : float), (stepper : float Seq.t)) =
    match stepper () with
    | Cons (x, xs) ->
        let nxt = x +. current |> Toolkit.wrapf min max in
        Some (current, (nxt, xs))
    | Nil -> None
  in
  let init = (min, step) in
  Seq.unfold update init

let always x = fun _ -> x

let osc_in = Array.init 1024 (always 0.0) 

let handle_int_float_arg datas =
  let data = Array.to_list datas in
  match data with
  | [`Int32 i;`Float64 j] -> 
    Some(i,j)
  | [`Int64 i;`Float64 j] ->
    Some (i,j)
  | _ -> None

let maybe_update setter opt = 
  match opt with
  | Some x -> setter x
  | None -> ()

let set_value arr (index,value) = 
  arr.(index) <- value


let handle_osc_message path message = 
  match path with (* handle the opt value *)
  | "table9" -> handle_int_float_arg message |> maybe_update (set_value osc_in)
  | _ -> ()


let oscReceiver =
  let open Osc_unix.Udp in
  let open Result in
  let localhost = Unix.inet_addr_of_string "127.0.0.1" in
  let port = 4568 in
  let addr = Unix.ADDR_INET (localhost, port) in
  let buffer_length = 1024 in
  let server = Server.create addr buffer_length in
  let server_receive_thread () =
    let rec loop () =
      let result = Server.recv server in
      match result with
      | Ok (message,addr) -> handle_osc_message addr message; loop ()
      | Error `Missing_typetag_string ->
         failwith "Missing typetag string"
      | Error (`Unsupported_typetag tag) ->
         failwith (Printf.sprintf "Unsupported typetag: %c" tag)
    in
    loop ()
  in
  Thread.create server_receive_thread 

let stream =
  let xref = ref (-1.0) in
  let accel = (rdRef xref) *.~ (st (1.0 /. (float_of_int (pow 2 14)))) in
  let speed = boundedwalk (-0.2) 0.2 accel in
  Cisp.rcRef xref (boundedwalk (-1.0) 1.0 speed)

let () = Jack.playSeqs 0 Process.sample_rate [stream *.~ (st 0.1);stream *.~ (st 0.1)]
