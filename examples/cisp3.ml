open Cisp
open Midi
open Seq

let sr = ref 44100.0

let channel = 1 


(* TODO recursive clock mapping *)            
(* this maps midi input msg to an output msg (raw midi) *)
let midiInputTestFun input =
  input
  |> map (fromMidiMsgWithDur (Samps 1000))
  |> map (mapOverPitch (fun p -> p + 0)) (* applicative ! *)
  |> withDur (ch [|42500|])
  |> withChan (st 4)
  |> withVelo (seq [100;0] |> hold ([st 1;seq [|2;3;1;1|]] |> ofList |> transcat))
  |> serialize |> map toRaw
                         
let () = Midi.playMidi midiInputTestFun sr 
