open Cisp
open Midi
open Seq
open Reader.Ops

(* simple mod of controller 1 onto pitch *)

let sr = ref 44100.0

let pitchControl =
  MidiState.getControlR (MidiCh 0) (MidiCtrl 0) 
  >>= (fun (MidiVal ctrl1) ->
    MidiState.getControlR (MidiCh 0) (MidiCtrl 1) >>=
      (fun (MidiVal ctrl2) ->
        MidiState.triggerR 3000 >>= (fun evt ->
         (ctrl1,ctrl2, evt) |> Reader.return)))
       
let offsetPitch offset evt =
  mapOverPitch ((+) offset) evt

let overwritePitch newPitch evt =
  withPitch newPitch evt

(* zipWith is awesome! it allows to combine two seqs into one! *)


let ofTuple tup =
  let (c1,c2,evt) = unzip3 tup in (* a seq of (x,y) make it (seq x, seq y) *)
  let arr = [|0;7;12;14;15;14|] in (* myseq *)
  let arr2 = [|0;7;4;4;0;12;-12|] in
  let mywalk = walki 0 c1 in
  let mywalk2 = walki 0 c2 in
  let indexed = index arr mywalk in
  let indexed2 = index arr2 mywalk2 in
  zipWith offsetPitch (indexed +~ indexed2) evt (* add the summed seqs to evt.pitch *)
  
(* this maps midi input msg to an output msg (raw midi) *)
let midiInputTestFun input =
  input 
  |> MidiState.makeSeq (* take msg, make it a state *)
  |> map (Reader.run pitchControl) (* run a bunch of readers to extract properties *)
  |> ofTuple (* the result is then used to contruct streams *)
  |> serialize |> map toRaw (* turn back into raw midi *)

  
let () = Midi.playMidi midiInputTestFun sr 
           
