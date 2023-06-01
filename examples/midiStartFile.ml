open Cisp
open Midi

let f opt = match opt with Some evt -> evt | None -> emptyBundle

let pattern =
  let starts =
    Infseq.seq [ 50; 54; 50; 50; 52; 50; 48; 55 ]
    |> Infseq.hold (Infseq.repeat 2)
    |> Infseq.map float_of_int
  in
  let steps =
    Infseq.repeat
      (Cisp.ch [| 0; 4; 12; 24 |] |> Cisp.take 10 |> Cisp.cyclen 4 |> floatify)
  in
  Infseq.map2
    (fun start seq -> seq |> Seq.map (fun x -> x +. start))
    starts steps
  |> Infseq.concatSq |> Infseq.map int_of_float

let mkChord lst start dur =
  chord
    (lst |> List.to_seq
    |> Seq.map (fun o ->
           c3
           |> withPitch (mkPitchClip (o + start))
           |> withDur (Samps (sec_to_samps dur))
           |> withVelo (mkVelocityClip (Toolkit.rvi 80 120))))

let durpat = Infseq.seq [ 0.01; 0.3; 0.3 ]

let chordStream =
  (* generate a sequence of chords *)
  let a = [ 0; 3; 7; 9; 14; 17 ] in
  let b = [ 0; 5; 7; 12; 17; 19 ] in
  let c = [ 0; 5; 10; 15 ] in
  let d = [ 0 ] in
  [ a; d; b; d; c; d ] |> List.to_seq
  |> Cisp.hold (Cisp.rv (st 1) (st 2))
  |> Infseq.cycleSq

let midiFun input =
  let p = Infseq.map3 mkChord chordStream pattern durpat |> Infseq.toSeq in
  let synced =
    input |> Seq.map (fun x -> x |> Midi.isNoteOn) |> fun t -> syncOverClock t p
  in
  let bundles = synced |> Seq.map f in
  bundles |> serializeBundles |> Seq.map toRaw

let () =
  let f () =
    Midi.playMidi midiFun Process.sample_rate;
    while true do
      Unix.sleep 60
    done
  in
  let _ = Thread.create f () in
  (* let _ =
    Unix.sleep 1;
    Sys.command "jack_connect ocaml_midi:ocaml_midi_out system_midi:playback_1"
  in
  let _ =
    Unix.sleep 1;
    Sys.command "jack_connect system_midi:capture_2 ocaml_midi:ocaml_midi_in"
  in *)
  while true do
    Unix.sleep 60
  done
