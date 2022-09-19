
(*
# Trying to write a live websocket interface for streams
 
Idea: send a string to ocaml, have it parsed and replace the existing stream.arr

Steps needed:

- walks ?
- busses ?

- merge jack, osc and jackmidi into one client

How to build sum types:
 should we do cmd1 cmd2 cmd3 nocommand or
 cmd Option.t ?
  
- ... 

Note: 'a Infseq.t is pretty similar to 'a ref.
The difference is that with Infseq.t you have to pass the rest part explicitely and the state is local, no funky action at a distance.
But if there is only one ref, this is fine.

*)

(* This creates a stream out of a ref *)


type model =
  Midi.midiNoteGenerator



let try_exec instruction =
  let evalled =
    Quip.eval_string_to_stream instruction 
  in
  match evalled with
  | Some (Quip.InfStream sq) -> Some (Infseq.to_seq sq)
  | Some (Quip.FinStream sq) -> Some sq
  | _ -> None


let handle_osc_argument arg =
  match arg with
  | Osc.OscTypes.String value ->
      ignore
        (print_endline "string par received:\n");
        (print_endline value);
         try_exec value |> (Option.to_result ~none:"parser error")
         
  | _ -> ignore (print_endline "some unexpected argument occured");
      Result.error "...ignoring a non-string arg..."

type parameter = 
  | Pitch
  | Velo
  | Duration
  | Channel

type stream_msg =
  Stream_message of parameter * float Seq.t

(* this is a kind of "mail box" for incoming updates *)
let current_msg : stream_msg Option.t ref = ref None

let custom_update model =
  match !current_msg with
  | None -> model
  | Some arg ->
      current_msg := None; (* Ok, we have processing this cmd, so unset it *)
      match arg with
      | Stream_message (Pitch,p) -> Midi.genWithPitch p model 
      | Stream_message (Velo,v) -> Midi.genWithVelo v model
      | Stream_message (Duration,d) -> Midi.genWithDur d model
      | Stream_message (Channel,c) -> Midi.genWithChannel c model

let option_and_then f opt = Option.bind opt f

let handle_packet packet socket =
  ignore socket;
  let make_stream_message arg_type sq =
    Stream_message (arg_type,sq)
  in
  match packet with
  | Osc.OscTypes.Message { address; arguments } ->
    begin
    let arg_type =
      match address with
      | "/pitch" -> Result.Ok Pitch
      | "/velo" -> Result.Ok Velo
      | "/duration" -> Result.Ok Duration
      | "/channel" -> Result.Ok Channel
      | _  -> Result.error ("unsupported address: " ^ address)
    in
    match arguments with
    | [] -> Result.error "zero arguments"
    | arg1 :: _ -> 
      let int_seq =
        handle_osc_argument arg1 
      in
      let ( >>= ) a f = Result.bind a f in
      arg_type >>= fun at ->
      int_seq >>= fun sq -> 
      Result.ok (make_stream_message at sq)
    end
  | _ -> Result.error "Bundles are not yet supported" 

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
          begin  
          let handled = handle_packet received_packet socket in
            match handled with
            | Ok r -> current_msg := (Some r)
            | Error err -> print_endline ("osc receiver error: " ^ err); current_msg := None
          end
          ;
          loop ()
        | Error `Missing_typetag_string -> failwith "Missing typetag string"
        | Error (`Unsupported_typetag tag) -> failwith (Printf.sprintf "Unsupported typetag: %c" tag)
    in
    loop ()
  in
  Thread.create server_receive_thread 






let generator = 
  let open Cisp in 
    Midi.MidiNoteGen {
        pitch = st 60;
        velo = List.to_seq [ 80; 60; 60 ] |> Seq.cycle;
        durInSec = List.to_seq [ 0.05 ] |> Seq.cycle;
        channel = st 1;
      }

let () =
  let f () =
    let gen = generator in 
    Midi.playMidi (Midi.from_dynamic_generator gen custom_update) Process.sample_rate;
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
  
  sendosc 127.0.0.1 4568 /test s "(transcat (hold (rv 3 5) (cycle 60 64 67)) (hold (rv 5 10) (cycle 67 69 72)) (hold (rv 5 10) (cycle 53 59 52)))"
  *)