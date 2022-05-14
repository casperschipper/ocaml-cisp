open Cisp
open Midi
open Seq

let sr = ref 44100.0

let channel = 1 

(* this maps midi input msg to an output msg (raw midi) *)
let midiInputTestFun input =
  input
  |> trigger (st c3)
  |> withPitch ([seq [77; 70; 63];seq [70;70;68;70;72] ;seq [66;67;68;69;70] |> hold (ch [|1;2;5|])] |> ofList |> transcat)
  |> withDur (seq [1000;2000;2000])
  |> withChan (st 4)
  |> withVelo (seq [100;30] |> hold (seq [1;2;1;3;2;1]))
  |> serialize |> map toRaw
                         
let () = Midi.playMidi midiInputTestFun sr 
