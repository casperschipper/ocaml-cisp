open Cisp
  
(*
# Trying to write a live websocket interface for streams

Idea: send a string to ocaml, have it parsed and replace the existing stream.arr

Steps needed:

- receive an OSC message
- receive a string and have it change a parameter 
- receive a string representing a list and have it inserted
   how to compile actual programs referencing functions from CISP?
- support more varients
- have multiple streams
- ... 


*)

(* This creates a stream out of a ref *)




let rec handleMess rf =
  ofRef rf |> 
    Seq.map (fun x -> 
        match x with
        | true -> rf := false ; 1.0
        | false -> 0.0
      )
  

let osc_in = ref false

type event =
  | NoteOn of { offset : int
           ; duration : int
           ; speed : float }
  | NoteOff of { velo : int }

  

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
      | Ok (_,_) -> osc_in := true; loop ()
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
