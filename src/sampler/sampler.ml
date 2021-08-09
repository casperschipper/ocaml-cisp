open OUnit
open OSC.OscTypes

let parsePacket _ =
  1.0 

let osc_to_stream =
  let open Lwt in
  let open Osc_lwt.Udp in
  let localhost = Unix.inet_addr_of_string "127.0.0.1" in
  let port = 4567 in
  let addr = Lwt_unix.ADDR_INET (localhost, port) in
  let buffer_length = 1024 in
  let state = ref 0.0 in
  let server = Server.create addr buffer_length in
  let handlePacket packet =
    match packet with
    | Ok (received_packet,_ ) ->
       received_packet |> parsePacket |> (fun p -> state := p);
       return ()
    | 
  let rec recLoop =
    Server.recv server >>= handlePacket >>= recLoop
  in
  let ofRef rf () = Seq.Cons (!rf, ofRef rf) in
  Lwt.async (Lwt_main.run recLoop)
; ofRef state
    


