open Cisp
open Midi
open Seq

let sr = ref 44100.0

let channel = 1 

(* this maps midi input msg to an output msg (raw midi) *)
let midiInputTestFun input =
  input
  |> trigger (mkRhythm (st c3) (seq [1;1;3;5]) (seq [2;3])) 
  |> withPitch (seq [60;64;76])
  |> withDur (ch [|425|])
  |> withChan (st 1)
  |> withVelo (st 100)
  |> serialize |> map toRaw
                         
let () = Midi.playMidi midiInputTestFun sr 
