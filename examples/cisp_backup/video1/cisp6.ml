open Cisp
open Midi
open Seq

let sr = ref 44100.0

let channel = 1

let maskEvt mask evtSq =
  let c = zip evtSq mask in
  map (fun (e,mask) -> if mask then e else SilenceEvent) c

let mkPattern sqA sqB nA nB =
  seq [sqA;sqB] |> hold (interleave nA nB)

let mask =
  mkPattern true false (seq [1;3;1;2]) (seq [2;3;1])
  
(* TODO recursive clock mapping *)            
(* this maps midi input msg to an output msg (raw midi) *)
let midiInputTestFun input =
  input
  |> map (fromMidiMsgWithDur (Samps 1000))
  |> maskEvt mask
  |> mapSeqOverPitch (fun x _ -> x + 60) (seq [12;0] |> hold (seq [5;2;3;1;1;1;3]))
  |> withDur (ch [|4250|])
  |> withChan (st 6)
  |> withVelo (st 100)
  |> serialize |> map toRaw
                         
let () = Midi.playMidi midiInputTestFun sr 
