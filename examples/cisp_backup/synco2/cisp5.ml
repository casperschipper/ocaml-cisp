open Cisp
open Midi
open Seq

let sr = ref 44100.0

let channel = 1 


            
(* this maps midi input msg to an output msg (raw midi) *)
let midiInputTestFun input =
  input
  |> trigger (st c3)
  |> withPitch (seq [60;61] |> hold ([hold (st 10) (st 1); hold (st 1) (st 2)] |> ofList |> transcat))
  |> withDur (ch [|425|])
  |> withChan (st 5)
  |> withVelo (st 100)
  |> serialize |> map toRaw
                         
let () = Midi.playMidi midiInputTestFun sr 
