open Cisp
open Midi
open Seq

(* simple mod of controller 1 onto pitch *)

let sr = ref 44100.0

type data = {c1: int; p: pitch; c2: int; c3: int; c4: int}

let currentState = ref {c1= 0; p= Pitch 0; c2= 0; c3= 0; c4= 0}

let pitchControl3 =
  let ( let* ) x f = Reader.bind f x in
  let* (MidiVal ctrl1) = MidiState.getControlR (MidiCh 0) (MidiCtrl 1) in
  let* (MidiVal ctrl2) = MidiState.getControlR (MidiCh 0) (MidiCtrl 2) in
  let* (MidiVal ctrl3) = MidiState.getControlR (MidiCh 0) (MidiCtrl 3) in
  let* (MidiVal ctrl4) = MidiState.getControlR (MidiCh 0) (MidiCtrl 4) in
  let* pitch = MidiState.getPitchR in
  let* trigger = MidiState.boolFromNote in
  (* create trigger from note On *)
  let () =
    currentState := {c1= ctrl1; p= pitch; c2= ctrl2; c3= ctrl3; c4= ctrl4}
  in
  (* write state ref *)
  Reader.return trigger

let scale_down y x = x |> float_of_int |> fun x' -> x' /. y

let sec2samp s = s |> seci |> fun x -> Samps x

let wlkr midiIn arr divider lens =
  let wlk =
    walk 0.0 (midiIn |> map lens |> map (scale_down divider)) |> trunc
  in
  let ixi = index arr wlk in
  ixi

let shrinkTill ratio start target =
  if ratio >= 1.0 then Seq.empty
  else
    Seq.unfold
      (fun state ->
        let next = state *. ratio in
        if next < target then None else Some (state, next))
      start

let growTill ratio start target =
  if ratio <= 1.0 then Seq.empty
  else
    Seq.unfold
      (fun state ->
        let next = state *. ratio in
        if next > target then None else Some (state, next))
      start

let mkScore delaySq pitchesSq =
  pitchesSq
  |> map2
       (fun delay off ->
         RelNote
           (Samps delay, transP off (mapOverDuration (fun _ -> seci 2.0) c3)))
       delaySq
  |> List.of_seq |> relToScore

(** everything needs to be a stream, this is too static *)
let ofTrigger trig =
  let stt = ofRef currentState in
  let l1 s = s.c1 in
  let l2 s = s.c2 in
  let l3 s = s.c3 in
  let entryDelays =
    stt
    |> map (fun s ->
           s.c4 |> float_of_int
           |> fun x -> x /. 128.0 |> ( *. ) 0.4 |> ( +. ) 0.004)
    |> map (fun entry ->
           [entry *. 1.0; entry *. 2.0; entry *. 1.0; entry *. 3.0]
           |> List.to_seq)
    |> concat
  in
  let combinedWalks =
    wlkr stt [|0; 7; 0; -7; 14|] 4.0 l1
    +~ wlkr stt [|0; 5; 10; 14; -7; 0|] 4.0 l2
    +~ wlkr stt [|-12; 0; 12; 0; -12|] 4.0 l3
  in
  let arpSq =
    combinedWalks
    |> batcher (st 12)
    |> map2 (fun entry score -> mkScore (st (seci entry)) score) entryDelays
    |> map Option.some
  in
  weavePattern trig arpSq (st None)

(* this maps midi input msg to an output msg (raw midi) *)
let midiInputTestFun input =
  input |> MidiState.makeSeq (* take msg, make it a state *)
  |> map (Reader.run pitchControl3)
  (* run a bunch of readers to extract properties *)
  |> ofTrigger
  |> playArp |> serializeBundles |> map toRaw

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
