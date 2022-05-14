open Cisp
open Midi
open Seq

let sr = ref 44100.0

let second x = x * 44100


            
(* this maps midi input msg to an output msg (raw midi) *)
let midiInputTestFun input =
  input
  |> trigger (mkRhythm c3 (st 1) (seq [7;5;1;1]))
  |> withPitch (seq [60;64;58;67;66;59;60;72;71])
  |> withDur (ch [|second 2|])
  |> withChan (st 6)
  |> withVelo (st 100)
  |> serialize |> map toRaw
                         
let () = Midi.playMidi midiInputTestFun sr 
