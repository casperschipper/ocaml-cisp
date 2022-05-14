open Cisp
open Midi
open Seq

let sr = ref 44100.0

let channel = 1 

(* this maps midi input msg to an output msg (raw midi) *)
let midiInputTestFun input =
  input
  |> trigger (mkRhythm  c3 (seq [1]) (seq [2;2;2;1;2;2;2;1])) 
  |> withPitch (seq [64;72])
  |> withDur (ch [|425|])
  |> withChan (st 2)
  |> withVelo (st 100)
  |> serialize |> map toRaw
                         
let () = Midi.playMidi midiInputTestFun sr 
