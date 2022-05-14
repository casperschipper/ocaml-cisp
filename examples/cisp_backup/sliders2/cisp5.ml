open Cisp
open Midi
open Seq

let sr = ref 44100.0

let channel = 1



(* TODO recursive clock mapping *)            
(* this maps midi input msg to an output msg (raw midi) *)
let midiInputTestFun input =
  input
  |> trigger (mkRhythm c3 (st 9) (st 1))
  |> mapSeqOverPitch (fun x y -> x + y) (seq [0;12] |> hold (seq [5;2;3;1;1;1;3]))
  |> withDur (ch [|4250|])
  |> withChan (st 5)
  |> withVelo (st 100)
  |> serialize |> map toRaw
                         
let () = Midi.playMidi midiInputTestFun sr 
