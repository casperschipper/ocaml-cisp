open Cisp
open Midi
open Seq


(* simple mod of controller 1 onto pitch *)

let sr = ref 44100.0

       (*
let pitchControl =
  MidiState.getControlR (MidiCh 0) (MidiCtrl 0) >>= fun (MidiVal ctrl1) ->
  MidiState.getControlR (MidiCh 0) (MidiCtrl 1) >>= fun (MidiVal ctrl2) ->
  MidiState.triggerR 3000 >>= fun evt ->
  (ctrl1,ctrl2, evt) |> Reader.return
        *)
let pitchControl2 =
  let (let*) x f = Reader.bind f x in
  let* (MidiVal ctrl1) = MidiState.getControlR (MidiCh 0) (MidiCtrl 2) in
  let* (MidiVal ctrl2) = MidiState.getControlR (MidiCh 0) (MidiCtrl 3) in
  let* evt = MidiState.triggerR 9000 in
  Reader.return (ctrl1,ctrl2, evt)
  
let offsetPitch offset evt =
  mapOverPitch ((+) offset) evt
 
(* LEAD MELODY *) 

  
(* let overwritePitch newPitch evt =
  mapOverPitch (fun _ -> newPitch) evt *)

(* zipWith is awesome! it allows to combine two seqs into one! *)

 
let ofTuple tup =
  let (c1,c2,evt) = unzip3 tup in (* a seq of (x,y) make it (seq x, seq y) *)

  let ctl1 = ref 0 in
  let ctl2 = ref 0 in

  let mywalk = walki 0 (ofRef ctl1) in
  let arr = [|0;2;4;5;7;12;4|] in
  let indexed = index arr mywalk in

  
  let mywalk2 = walki 0 (ofRef ctl2) in
  let arr2 = [|(-12);0;7;12;24;7|] in
  let indexed2 = index arr2 mywalk2 in
  
  evt
  |> effect (wrRef ctl1 c1)
  |> effect (wrRef ctl2 c2)
  |> overwritePitch (indexed +~ indexed2 +~ (st 60))
  |> overwriteVelo (seq [0; 100])
  |> overwriteChan (st 2)
  
(* this maps midi input msg to an output msg (raw midi) *)
let midiInputTestFun input =
  input
  |> MidiState.makeSeq (* take msg, make it a state *)
  |> map (Reader.run pitchControl2) (* run a bunch of readers to extract properties *)
  |> ofTuple (* the result is then used to contruct streams *)
  |> serialize |> map toRaw (* turn back into raw midi *)

  
let () = Midi.playMidi midiInputTestFun sr 
           
