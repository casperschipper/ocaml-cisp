open Cisp
open Midi
open Seq

(* simple mod of controller 1 onto pitch *)

let sr = ref 44100.0

type data = {c1: int; p: pitch; c2: int}

let currentState = ref {c1= 0; p= Pitch 0; c2= 0}

let pitchControl3 =
  let ( let* ) x f = Reader.bind f x in
  let* (MidiVal ctrl1) = MidiState.getControlR (MidiCh 0) (MidiCtrl 1) in
  let* (MidiVal ctrl2) = MidiState.getControlR (MidiCh 0) (MidiCtrl 2) in
  let* pitch = MidiState.getPitchR in
  let* trigger = MidiState.boolFromNote in
  (* create trigger from note On *)
  let () = currentState := {c1= ctrl1; p= pitch; c2= ctrl2} in
  (* write state ref *)
  Reader.return trigger

let scale_down y x = x |> float_of_int |> fun x' -> x' /. y

let map2 f xs ys = zipWith f xs ys

let sec2samp s = s |> seci |> fun x -> Samps x

let ofTrigger trig =
  let midiIn = ofRef currentState in
  let myWalk =
    walk 0.0 (midiIn |> map (fun state -> state.c1 |> scale_down 4.0)) |> trunc
  in
  let arr = [|0; 5; 10; 15|] in
  let ixi = index arr myWalk in
  let myWalk2 =
    walk 0.0 (midiIn |> map (fun state -> state.c2 |> scale_down 4.0)) |> trunc
  in
  let arr2 = [|0; 7; 14; 21; 28; 35; 42|] in
  let ixi2 = index arr2 myWalk2 in
  let notes =
    zipToNoteEvt (MidiCh 1 |> st)
      (ixi |> ( +~ ) (st 20) |> map mkPitchClip)
      ([100; 80; 90; 100; 70] |> List.map mkVelocityClip |> seq)
      ([|sec2samp 0.2; sec2samp 0.1|] |> ch)
  in
  let bundles =
    notes
    |> map2
         (fun offset x ->
           [x; transposePitch offset x; transposePitch (offset * 2) x]
           |> List.to_seq)
         (ixi2 |> hold (seq [2; 3; 5]))
    |> Seq.map chord
  in
  let silence = st silenceBundle in
  weavePattern trig bundles silence

(* this maps midi input msg to an output msg (raw midi) *)
let midiInputTestFun input =
  input |> MidiState.makeSeq (* take msg, make it a state *)
  |> map (Reader.run pitchControl3)
  (* run a bunch of readers to extract properties *)
  |> ofTrigger
  |> serializeBundles |> map toRaw

(* turn back into raw midi
let testIn =
  let s = SilenceEvent in
  [c3; s; s; s; s; s; s; s; s; s; s]
  |> seq
  |> map (fun note -> [note; mapOverPitch (( + ) 12) note] |> List.to_seq)
  |> Seq.map chord |> take 30

let () =
  testIn |> serializeBundles |> map toRaw
  |> Seq.fold_left
       (fun () x -> print_string "\nraw =" ; printRaw x ; print_string "\n\n")
       ()
     *)

let () = Midi.playMidi midiInputTestFun sr
