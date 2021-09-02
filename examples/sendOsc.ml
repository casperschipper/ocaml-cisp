

let bangMessage =
    Osc.OscTypes.Message {
        address = "/test";
        arguments = [ Osc.OscTypes.String "bang!" ];
      }



let midiFun channel input =
  let map = Seq.map in
  let open Midi in
  input
  |> MidiState.makeSeq (* make a running state seq : MidiState.t Seq.t *)
  |> map (Reader.run MidiState.boolFromNote) (* test for note : Bool Seq.t  *)
  |> map (fun trig -> (* handle the triggers, if trigger inform the osc sender *) 
         match trig with
         | true -> let _ = Event.sync (Event.send channel ()) in
                    SilenceEvent
         | false -> SilenceEvent ) (* whatever happens, we don't produce midi, we only want the side effect *)
  |> Midi.serialize
  |> map toRaw
  

let () =
  let open Osc_unix.Udp in
  let localhost = Unix.inet_addr_of_string "127.0.0.1" in
  let port = 5665 in
  let addr = Unix.ADDR_INET (localhost, port) in
  let client = Client.create () in
    
  let channel = Event.new_channel () in (* make a channel *)
  let rec receive_event channel = (* a thread that just waits for the event, then sends a thing *)
    let () =
      Event.sync (Event.receive channel)
    in
     Client.send client addr bangMessage;receive_event channel (* ok when done, wait for the next one *)
  in
  
  let f channel = 
    Midi.playMidi (midiFun channel) Process.sample_rate 
    ;while true
     do Unix.sleep 60 done

  in
  let _ = Thread.create receive_event channel in
  let (_:Thread.t) = Thread.create f channel in
  let _ = Sys.command "jack_disconnect system_midi:capture_2 ocaml_midi:ocaml_midi_in" in
  let _ = Sys.command "jack_disconnect ocaml_midi:ocaml_midi_out system_midi:playback_1" in
  let _ = Sys.command "jack_connect ocaml_midi:ocaml_midi_out system_midi:playback_1" in
  let _ = Sys.command "jack_connect system_midi:capture_2 ocaml_midi:ocaml_midi_in" in
  while true
  do
    Unix.sleep 60
  done
 
