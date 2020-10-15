open Cisp
open Midi
open Seq
open Reader.Ops

(* simple mod of controller 1 onto pitch *)

let sr = ref 44100.0

let pitchControl =
  MidiState.getControlR (MidiCh 0) (MidiCtrl 0) 
  >>= (fun (MidiVal ctrl1) ->
    MidiState.triggerR 3000 >>= (fun evt ->
         (ctrl1, evt) |> Reader.return))

let offsetPitch offset evt =
  mapOverPitch ((+) offset) evt

let overwritePitch newPitch evt =
  withPitch newPitch evt

(* zipWith is awesome! it allows to combine two seqs into one! *)


let ofTuple tup =
  let (ctrl, evt) = unzip tup in (* a seq of (x,y) make it (seq x, seq y) *)
  let arr = [|0;7;12;14;15;14|] in (* myseq *)
  let mywalk = walki 0 ctrl in 
  let indexed = index arr mywalk in
  zipWith offsetPitch indexed evt (* add the summed seqs to evt.pitch *)
  
(* this maps midi input msg to an output msg (raw midi) *)
let midiInputTestFun input =
  input 
  |> MidiState.makeSeq (* take msg, make it a state *)
  |> map (Reader.run pitchControl) (* run a bunch of readers to extract properties *)
  |> ofTuple (* the result is then used to contruct streams *)
  |> serialize |> map toRaw (* turn back into raw midi *)

  
let () = Midi.playMidi midiInputTestFun sr 
           
