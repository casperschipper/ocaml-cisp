open Cisp
open Midi
open Seq


(* simple mod of controller 1 onto pitch *)

let sr = ref 44100.0

let pitchControl2 =
  let (let*) x f = Reader.bind f x in
  let* (MidiVal ctrl1) = MidiState.getControlR (MidiCh 0) (MidiCtrl 0) in
  let* (MidiVal ctrl2) = MidiState.getControlR (MidiCh 0) (MidiCtrl 1) in
  let* evt = MidiState.triggerR 9000 in
  Reader.return  (ctrl1,ctrl2,evt)
  
let offsetPitch offset evt =
  mapOverPitch ((+) offset) evt
 
(* LEAD MELODY *) 

  
(* let overwritePitch newPitch evt =
  mapOverPitch (fun _ -> newPitch) evt *)

(* zipWith is awesome! it allows to combine two seqs into one! *)

(* there is a stream of state, which we want to merge into something else *)

    (* 
  n - - - - - n - - - - - n

  s s s s s s s s s s s s s s

     *) 

let ofTuple tup =
  let (c1,c2,evt) = unzip3 tup in (* tuple of seq.t to 3 seq.t *)

  let ctl1 = ref 0 in
  let ctl2 = ref 0 in

  let mywalk = walki 0 (ofRef ctl1) in
  let arr = [|0;7;14|] in
  let indexed = index arr mywalk in
  
  let mywalk2 = walki 0 (ofRef ctl2) in
  let arr2 = [|(-12);0;12|] in
  let indexed2 = index arr2 mywalk2 in
  
  evt
  |> effect (wrRef ctl1 c1)
  |> effect (wrRef ctl2 c2)
  |> overwriteVelo (seq [0;100])
  |> overwritePitch (indexed +~ indexed2 +~ (st 60))

  
(* this maps midi input msg to an output msg (raw midi) *)
let midiInputTestFun input =
  input
  |> MidiState.makeSeq (* take msg, make it a state *)
  |> map (Reader.run pitchControl2) (* run a bunch of readers to extract properties *)
  |> ofTuple (* the result is then used to contruct streams *)
  |> serialize |> map toRaw (* turn back into raw midi *)

  
let () = Midi.playMidi midiInputTestFun sr 
           
