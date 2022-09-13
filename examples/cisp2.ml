open Cisp
open Midi

let map = Seq.map


let walkFromN stepSq startSq numberSq =
    let chunkedSteps = stepSq |> group numberSq in 
    walki <$> startSq <*> chunkedSteps |> concat

(* interseting note, the steps are consumed and so should be passed on to the next session *)

let generator =
  (*let combin =  
    let starts = walkFromN (seq [7;5]) (seq [0;2;0;1;0;-1]) (seq [2;3;4]) |> hold (seq [2;3;1;1;1]) in
    let number = seq [5;4;5;2;2] in 
    let step = seq [-12;12] |> hold (seq [10;1;1;1;1;1;1;2]) in
    walkFromN step starts number
  in*)
  MidiNoteGen
    { pitch= (st 60)
    ; velo= seq [80;60;60] 
    ; durInSec= seq [0.8;0.3;0.2]
    ; channel= st 1 }

  


let () =
  let f () =
    Midi.playMidi (fromGenerator generator) Process.sample_rate ;
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
