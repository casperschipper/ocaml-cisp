module S = Lo.Server
module R = Result

type preset = Preset of {start_t: float; end_t: float}

type msg = Set of {voiceNr: int; preset: preset}

type data = Lo.Message.data

type state = Presets of preset Option.t Array.t

let myState = Array.make 128 None

let ofData = function
  | `Int32 n -> string_of_int n
  | `Float f | `Double f -> Printf.sprintf "%f" f
  | `String s -> s
  | `True -> "True"
  | `False -> "False"
  | _ -> "???"

type 'a oscParser =
  | OscParser of (data Seq.t -> ('a * data Seq.t, string) Result.t)

let run (OscParser p) osc = p osc

(* which is a monad *)
let return a = OscParser (fun s -> Result.ok (a, s))

(*   fmap f (Parser cs) = Parser (\s -> [(f a, b) | (a, b) <- cs s]) *)
let fmap f (OscParser p) =
  let mapFst f (a, s) = (f a, s) in
  OscParser (fun s -> p s |> Result.map (mapFst f))

(*
    
let join_result res_sq =
  let module R = Result in
  let rec aux in_sq acc =
    match acc with
    | Error err -> Error err
    | Ok accu ->
      begin
        match in_sq () with
        | Seq.Nil -> Ok accu
        | Seq.Cons(R.Ok sq,tail) ->
          let newAccu = R.Ok (Seq.append sq accu)in
          aux tail newAccu
        | Seq.Cons(R.Error err,_) ->
          R.Error err
      end
  in
  aux res_sq (R.Ok Seq.empty)
*)

let bind p f =
  let step result =
    match result with
    | R.Ok (a, rest) -> run (f a) rest
    | R.Error error -> R.Error error
  in
  OscParser
    (fun s ->
      let r1 = run p s in
      step r1 )

let ( >>= ) = bind

(* and applicative *)
let ( >> ) m k = m >>= fun _ -> k

let ( <*> ) fP xP = fP >>= fun f -> xP >>= fun x -> return (f x)

let failure err = OscParser (fun _ -> Result.Error err)

let item =
  let open Seq in
  OscParser
    (fun oscSq ->
      match oscSq () with
      | Nil -> Result.error "reached unexpected end"
      | Cons (x, xs) -> Result.Ok (x, xs) )

let satisfy predicate error =
  item
  >>= fun c ->
  print_string ("some data" ^ ofData c ^ "\n") ;
  flush stdout ;
  if predicate c then return c else failure error

let option p q =
  OscParser
    (fun s ->
      let res = run p s in
      match res with R.Error _ -> run q s | R.Ok res -> R.Ok res )

let isString str data = match data with `String s -> s = str | _ -> false

let float =
  item
  >>= fun d ->
  match d with
  | `Float f | `Double f -> return f
  | _ -> failure "expected a float"

let int =
  item
  >>= fun d ->
  match d with `Int32 i -> return i | _ -> failure "expected an int"

let floatParameter parName =
  let err = "Expected parameter name: " ^ parName in
  satisfy (isString parName) err >> float

let intParameter parName =
  let err = "Expected int parameter withname: " ^ parName in
  satisfy (isString parName) err >> int

let message =
  let mk voice_nr start_t end_t =
    Set {voiceNr= voice_nr; preset= Preset {start_t; end_t}}
  in
  intParameter "voiceNr"
  >>= fun n ->
  floatParameter "start"
  >>= fun start_t ->
  floatParameter "end"
  >>= fun end_t ->
  print_float end_t ;
  print_string "endt" ;
  flush stdout ;
  let made = mk n start_t end_t in
  return made

let update (Set {voiceNr; preset}) () =
  print_endline "write!" ;
  myState.(voiceNr) <- Some preset

let print_preset (Preset p) =
  print_string "start t:" ;
  print_float p.start_t ;
  print_string "end t:" ;
  print_float p.end_t ;
  print_newline () ;
  flush stdout

let print_state () =
  print_endline "ok" ;
  Array.iteri
    (fun idx opt ->
      match opt with
      | Some p ->
          print_string ("preset:\n" ^ string_of_int idx) ;
          print_preset p ;
          flush stdout
      | None -> () )
    myState

let testData =
  [ `String "voiceNr"
  ; `Int32 2
  ; `String "start"
  ; `Float 3.14
  ; `String "end"
  ; `Float 50.333 ]
  |> List.to_seq

let handler path data =
  let s = function
    | `Int32 n -> string_of_int n
    | `Float f | `Double f -> Printf.sprintf "%f" f
    | `String s -> s
    | `True -> "True"
    | `False -> "False"
    | _ -> "???"
  in
  let data = Array.to_list data in
  let s = List.map s data in
  let s = String.concat ", " s in
  Printf.printf "Message on %s: %s\n%!" path s

let myHandler path d =
  handler path d ;
  match path with
  | "/set" -> (
      let data = Array.to_seq d in
      let parsed = run message data in
      match parsed with
      | R.Error error ->
          print_string (error ^ "\n") ;
          flush stdout
      | R.Ok (m, _) -> update m () ; print_state () )
  | "/print" -> print_state () ; flush stdout
  | _ ->
      print_string ("unknown path: " ^ path ^ "\n") ;
      flush stdout

let () =
  let port = 7777 in
  let s = S.create port myHandler in
  while true do
    S.recv s
  done
