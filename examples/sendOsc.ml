

let bangMessage addr =
  Lo.send addr "/test/" [`Float 3.141592;`String "fish";`Float 2.233;`String "cat";`Float 3.33]


let midiFun addr input =
  let map = Seq.map in
  let open Midi in
  input
  |> MidiState.makeSeq (* make a running state seq : MidiState.t Seq.t *)
  |> map (Reader.run MidiState.boolFromNote) (* test for note : Bool Seq.t  *)
  |> map (fun trig -> (* handle the triggers, if trigger inform the osc sender *) 
         match trig with
         | true -> bangMessage addr ;
                    SilenceEvent
         | false -> SilenceEvent ) (* whatever happens, we don't produce midi, we only want the side effect *)
  |> Midi.serialize
  |> map toRaw
  

let () =
  (* let open Osc_unix.Udp in
  let localhost = Unix.inet_addr_of_string "127.0.0.1" in
  let port = 5665 in
  let addr = Unix.ADDR_INET (localhost, port) in
  let client = Client.create () in
   *)
   let addr = Lo.Address.create "127.0.0.1" 5665 in
    
   let f addr = 
    Midi.playMidi (midiFun addr) Process.sample_rate 
    ;while true
     do Unix.sleep 60 done
   in
  let (_:Thread.t) = Thread.create f addr in
  
  let _ = Sys.command "jack_disconnect system_midi:capture_2 ocaml_midi:ocaml_midi_in" in
  let _ = Sys.command "jack_disconnect ocaml_midi:ocaml_midi_out system_midi:playback_1" in
  let _ = Sys.command "jack_connect ocaml_midi:ocaml_midi_out system_midi:playback_1" in
  let _ = Sys.command "jack_connect system_midi:capture_2 ocaml_midi:ocaml_midi_in" in
  while true
  do
    Unix.sleep 60 
  done
 
