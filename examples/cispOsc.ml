open Cisp

let rec handleMess rf () =
  let parse x =
    match x with
    | true -> rf := false; 1.0
    | false -> 0.0
  in
  Seq.Cons( parse !rf, handleMess rf )
  

let osc_in = ref false

type event =
  | NoteOn { offset : int
           ; duration : int
           ; speed }
  | NoteOff 

let noteOn m =
  match m.arguments with
    
  

let message m =
  match m.address with
  "/noteOn" -> 

let handlePacket packet =
  match packet with
  | Message msg -> message msg
  | Bundle bundle ->

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
      | Ok (packet,socketAddr) -> osc_in := true; loop ()
      | Error `Missing_typetag_string ->
         failwith "Missing typetag string"
      | Error (`Unsupported_typetag tag) ->
         failwith (Printf.sprintf "Unsupported typetag: %c" tag)
    in
    loop ()
  in
  Thread.create server_receive_thread  
  
let () =
  let _ = oscReceiver () in
  let pulse = handleMess osc_in in
  Jack.playSeqs 2 Process.sample_rate [pulse; osc (st 220.0) ]
