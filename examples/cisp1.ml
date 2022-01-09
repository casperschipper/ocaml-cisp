open Cisp
open Midi

let map = Seq.map

type midiNoteGenerator =
  MidiNoteGen of { pitch : int Seq.t
           ; velo : int Seq.t
           ; durInSec : float Seq.t
           ; channel : int Seq.t }

let generator =
  MidiNoteGen { pitch = seq [10;22;23]
  ; velo = st 100 
  ; durInSec = seq [0.1;0.2]
  ; channel = st 1 }
  
let midiFun (MidiNoteGen {pitch;velo;durInSec;channel}) input =
  let durInSamp = map (function s -> s |> ( *. ) !Process.sample_rate |> int_of_float) durInSec in
  let stream =
    Seq.return makeNoteOfInts <*> pitch <*> velo <*> durInSamp <*> channel
  in
    trigger stream input |> serialize |> map toRaw

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
