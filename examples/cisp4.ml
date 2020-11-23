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
  mapOverPitch (fun _ -> newPitch) evt

(* zipWith is awesome! it allows to combine two seqs into one! *)


let ofTuple tup =
  let (c1,c2,evt) = unzip3 tup in (* a seq of (x,y) make it (seq x, seq y) *)
  (* let arr = [|0;2;4;5|] in (* myseq *)
 
  let mywalk = walki 0 c1 in
 
  let indexed = index arr mywalk in*)
  let cMod = map (fun x -> x - (x mod 12)) c2 in
  let cGate = map (fun x -> if x > 0 then 1 else 0) c1 in
 
  zipWith overwritePitch (((seq [0;2;4;5]) *~ cGate) +~ cMod +~ (st 60)) evt |> overwriteVelo (seq [0;100] |> hold (seq [2;3;2;2;2;3;3;3;2;1;2])) (* add the summed seqs to evt.pitch *)
  
(* this maps midi input msg to an output msg (raw midi) *)
let midiInputTestFun input =
  input 
  |> MidiState.makeSeq (* take msg, make it a state *)
  |> map (Reader.run pitchControl) (* run a bunch of readers to extract properties *)
  |> ofTuple (* the result is then used to contruct streams *)
  |> serialize |> map toRaw (* turn back into raw midi *)

  
let () = Midi.playMidi midiInputTestFun sr 
           
