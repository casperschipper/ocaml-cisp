
(*
# Trying to write a live websocket interface for streams

Idea: send a string to ocaml, have it parsed and replace the existing stream.arr

Steps needed:

- have some midi loop running
- receive a string and have it change a parameter 
- receive a string representing a list and have it inserted
- support more varients
- have multiple streams
- merge jack, osc and jackmidi into one client

- ... 

Note: 'a Infseq.t is pretty similar to 'a ref.
The difference is that with Infseq.t you have to pass the rest part explicitely and the state is local, no funky action at a distance.
But if there is only one ref, this is fine.

*)

(* This creates a stream out of a ref *)

let the_stream = ref None

type model =
  int Infseq.t

let try_exec instruction =
  let evalled =
    Quip.eval_string_to_stream instruction 
  in
  match evalled with
  | Some (Quip.InfStream sq) -> Some (Infseq.to_seq sq)
  | Some (Quip.FinStream sq) -> Some sq
  | _ -> None


let handle_argument arg =
  match arg with
  | Osc.OscTypes.String value ->
      ignore
        (print_endline "string par received:\n");
        (print_endline value);
         try_exec value
         
  | _ -> ignore (print_endline "some unexpected argument occured");
      None

let handle_packet packet socket =
  ignore socket;
  match packet with
  | Osc.OscTypes.Message { address; arguments } ->
      (ignore address;
      match arguments with
      | arg1 :: _ -> (handle_argument arg1)
      | [] -> None)
  | _ -> None

let oscReceiver  =
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
      | Ok (received_packet, socket) ->
          the_stream := handle_packet received_packet socket;
          loop ()
      (* | Ok (_,_) -> osc_in := true; loop () *)
      | Error `Missing_typetag_string -> failwith "Missing typetag string"
      | Error (`Unsupported_typetag tag) ->
          failwith (Printf.sprintf "Unsupported typetag: %c" tag)
    in
    loop ()
  in
  Thread.create server_receive_thread 

let rec quip_stream tail () =
  match !the_stream with
  | Some new_stream ->
    the_stream := None;
    (match new_stream () with
    | Seq.Cons(h,tl) -> Seq.Cons(h, quip_stream tl )
    | Nil -> Seq.Nil)
  | None -> 
      match tail () with
      | Seq.Cons(h,tl) -> Seq.Cons(h, quip_stream tl)
      | Seq.Nil -> Seq.Nil

    



let generator = 
  let open Cisp in 
    Midi.MidiNoteGen {
        pitch = quip_stream (st 60.0) |> Seq.map int_of_float;
        velo = List.to_seq [ 80; 60; 60 ] |> Seq.cycle;
        durInSec = List.to_seq [ 0.1;0.2;0.4 ] |> Seq.cycle;
        channel = Seq.forever (fun () -> 1);
      }

let () =
  let f () =
    Midi.playMidi (Midi.fromGenerator generator) Process.sample_rate;
    while true do
      Unix.sleep 60
    done
  in
  let _ = Thread.create oscReceiver () in
  let _ = Thread.create f () in
  let _ =
    Sys.command "jack_connect ocaml_midi:ocaml_midi_out system_midi:playback_1"
  in
  let _ =
    Sys.command "jack_connect system_midi:capture_2 ocaml_midi:ocaml_midi_in"
  in
  while true do
    Unix.sleep 60
  done

  (* Install sendosc command tool and run
  sendosc 127.0.0.1 4568 /test s "(hold (ch 72 84) (cycle 1 2 3))"   
  
  *)