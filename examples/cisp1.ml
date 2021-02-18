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

let pulseDivider divider sq =
  let rec aux n divider sq () =
    match sq () with
    | Cons (true, tail) -> (
        if n > 0 then Cons (false, aux (n - 1) divider tail)
        else
          match divider () with
          | Cons (curDiv, divTail) -> Cons (true, aux curDiv divTail tail)
          | Nil -> Nil )
    | Cons (false, tail) -> Cons (false, aux n divider tail)
    | Nil -> Nil
  in
  aux 0 divider sq

let scale_down y x = x |> float_of_int |> fun x' -> x' /. y

let ofTrigger trig =
  let midiIn = ofRef currentState in
  let myWalk =
    walk 0.0 (midiIn |> map (fun state -> state.c1 |> scale_down 4.0)) |> trunc
  in
  let arr = [|16; 12; 8; 4; 0|] in
  let ixi = index arr myWalk in
  let myWalk2 =
    walk 0.0 (midiIn |> map (fun state -> state.c2 |> scale_down 4.0)) |> trunc
  in
  let arr2 = [|0; 2; -2; -7; 7|] in
  let ixi2 = index arr2 myWalk2 in
  let notes =
    zipToNoteEvt (MidiCh 1 |> st)
      (ixi |> ( +~ ) (st 40) |> ( +~ ) ixi2 |> map mkPitchClip)
      (Velo 100 |> st) (Samps 4000 |> st)
  in
  weavePattern trig notes (st SilenceEvent)

(* this maps midi input msg to an output msg (raw midi) *)
let midiInputTestFun input =
  input |> MidiState.makeSeq (* take msg, make it a state *)
  |> map (Reader.run pitchControl3)
  (* run a bunch of readers to extract properties *)
  |> pulseDivider ([1] |> seq)
  |> ofTrigger |> serialize |> map toRaw

(* turn back into raw midi *)

let () = Midi.playMidi midiInputTestFun sr
