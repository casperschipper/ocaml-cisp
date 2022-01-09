open Cisp
open Midi

let map = Seq.map

let midiFun input =
  trigger (st c4) input |> serialize |> map toRaw

let () =
  let f () =
    Midi.playMidi midiFun Process.sample_rate ;
    while true do
      Unix.sleep 60
    done
  in
  let _ = Thread.create f () in
  let _ =
    Unix.sleep 1;
    Sys.command "jack_connect ocaml_midi:ocaml_midi_out system_midi:playback_1"
  in
  let _ =
    Unix.sleep 1;
    Sys.command "jack_connect system_midi:capture_2 ocaml_midi:ocaml_midi_in"
  in 
  while true do
    Unix.sleep 60
  done
 