open Cisp
open Midi

let map = Seq.map

type midiNoteGenerator =
  | MidiNoteGen of
      { pitch: int Seq.t
      ; velo: int Seq.t
      ; durInSec: float Seq.t
      ; channel: int Seq.t }

let generator =
  MidiNoteGen
    { pitch=
        st 34
        |> concatMap (fun start -> walki start (ch [|2;1;-1|]) |> take 3)
        |> concatMap (fun step -> walki step (seq [0; 1; 2;0;1;1;3]) |> take (rvi 3 8))
    ; velo= seq [100;100;70;100;80]
    ; durInSec= seq [0.2]
    ; channel= st 1 }

let midiFun (MidiNoteGen {pitch; velo; durInSec; channel}) input =
  let durInSamp =
    map
      (function
        | s -> s |> ( *. ) !Process.sample_rate |> int_of_float )
      durInSec
  in
  let stream = st makeNoteOfInts <*> pitch <*> velo <*> durInSamp <*> channel in
  input |> trigger stream |> serialize |> map toRaw

let () =
  let f () =
    Midi.playMidi (midiFun generator) Process.sample_rate ;
    while true do
      Unix.sleep 60
    done
  in
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
