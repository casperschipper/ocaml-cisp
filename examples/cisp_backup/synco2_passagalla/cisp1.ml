open Cisp
open Midi
open Seq

let sr = ref 44100.0

let channel = 1 


(* TODO recursive clock mapping *)            
(* this maps midi input msg to an output msg (raw midi) *)
let midiInputTestFun input =
  input
  |> trigger (st c3)
  |> withPitch ((iterwalk 72 (fun x y -> x + y) ([st (-4);st (-3);st (-4); (ch [|(-3);4;3;12|])] |> ofList |> transcat)) |> hold (st 12))
  |> withDur (ch [|425|])
  |> withChan (st 12)
  |> withVelo (st 100)
  |> serialize |> map toRaw
                         
let () = Midi.playMidi midiInputTestFun sr 
