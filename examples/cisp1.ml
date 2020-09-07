open Cisp
open Midi
open Seq

let sr = ref 44100.0

let channel = 1 

(* this maps midi input msg to an output msg (raw midi) *)
let midiInputTestFun input =
  (* this translates input into boolean trigger *)
  (* a metre boolean mask that triggers notes or rests in the pattern *)
  (* dummy event *)
  (* apply metre to default event, use Silence as filler *)
  input 
  |> map (fromMidiMsgWithDur (Samps 4200))
  |> withDur (ch [|4250|])
  |> withChan (st 1)
  |> withVelo (st 100)
  |> serialize |> map toRaw


let () = Midi.playMidi midiInputTestFun sr
