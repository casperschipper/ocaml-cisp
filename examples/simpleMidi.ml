open Midi



let f inp =
  let t = inp |> Seq.map isNoteOn in
  Cisp.weavePattern t (Cisp.st (c3 |> withDur (Samps 40)) |> Seq.map2 Midi.transP (Seq.cycle (List.to_seq [0;4;7]))) (Cisp.st SilenceEvent)
  |> serialize |> Seq.map toRaw

let () =
  let f () =
    Midi.playMidi f Process.sample_rate;
    while true do
      Unix.sleep 60
    done
  in
  let _ = Thread.create f () in
  let _ = 
    print_int (Sys.command "jack_connect ocaml_midi:ocaml_midi_out system_midi:playback_1");
    print_int (Sys.command "jack_connect system_midi:capture_2 ocaml_midi:ocaml_midi_in");
    ignore(Sys.command "jack_lsp -c -A | grep ocaml");
  in
  while true do
    Unix.sleep 30
  done

