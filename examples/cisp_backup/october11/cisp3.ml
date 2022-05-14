open Cisp
open Midi
open Seq
open Reader.Ops

(* simple mod of controller 1 onto pitch *)

let sr = ref 44100.0

let pitchControl =
  MidiState.getControlR (MidiCh 0) (MidiCtrl 1)
  >>= (fun (MidiVal ctrl1) ->
    MidiState.triggerR 3000 >>= (fun evt ->
         (ctrl1, evt) |> Reader.return))

let ofTuple sq =
  let (ctrl, evt) = unzip sq in
  let sq = seq [0;7;12;14;15;14] in
  let plus = zipWith (+) sq ctrl in
  zip plus evt
  
  
(* TODO recursive clock mapping *)            
(* this maps midi input msg to an output msg (raw midi) *)
let midiInputTestFun input =
  input
  |> MidiState.makeSeq
  |> map (Reader.run pitchControl)
  |> ofTuple |> map (fun (offset, evt) -> mapOverPitch (fun p -> p + offset) evt)
  |> serialize
  |> map toRaw

  
let () = Midi.playMidi midiInputTestFun sr 
           
