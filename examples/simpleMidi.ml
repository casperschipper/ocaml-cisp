open Midi
open Cisp

let f inp =
  let stream = 
    mkNoteClip <$> (st 1)
    <*> (Seq.map2 (+) (Cisp.seq [0;4;7]) (st 60))
    <*> (seq [100;80;40;80])
    <*> (seq [1000;200;20])


  in
    Midi.trigger stream inp
  |> serialize |> Seq.map toRaw

let () =
  let f () =
    Midi.playMidi f Process.sample_rate ;
    while true do
      Unix.sleep 60
    done
  in
  let _ = Thread.create f () in
  let _ =
    print_int
      (Sys.command
         "jack_connect ocaml_midi:ocaml_midi_out system_midi:playback_1" ) ;
    print_int
      (Sys.command "jack_connect system_midi:capture_2 ocaml_midi:ocaml_midi_in") ;
    ignore (Sys.command "jack_lsp -c -A | grep ocaml")
  in
  while true do
    Unix.sleep 30
  done
