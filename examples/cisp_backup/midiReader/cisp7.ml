open Cisp
open Midi
open Seq

let sr = ref 44100.0

let second x = x *. 44100.


            
(* this maps midi input msg to an output msg (raw midi) *)
let midiInputTestFun input =
  input
  |> trigger (mkRhythm c3 (seq [2;3;3]) (seq [1;3;1]))
  |> withPitch ([seq [63;64;67;59];st 62;st 60] |> ofList |> transcat |> hold (seq [1;1;1;1;1;1;1;2;1;1;1;1;1;1;2;2;1;1;1;1;1;1;1;2;1;1;1;1;1;2]))
  |> withDur (ch [|second 0.1|] |> trunc)
  |> withChan (st 7)
  |> withVelo (st 100)
  |> serialize |> map toRaw
                         
let () = Midi.playMidi midiInputTestFun sr 
