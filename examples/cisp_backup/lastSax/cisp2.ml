open Cisp
open Midi
open Seq

(* simple mod of controller 1 onto pitch *)

let sr = ref 44100.0

type data =
  { (* type of data we want to read from midi input*)
    c1: int
  ; p: pitch
  ; c2: int
  ; c3: int
  ; c4: int
  ; c5: int
  ; c6: int
  ; c7: int }

let currentState =
  (* init *)
  ref {c1= 0; p= Pitch 0; c2= 0; c3= 0; c4= 1; c5= 1; c6= 1; c7= 1}

let midiParser =
  (* this combines a bunch of Reader's to extract values from incoming midi bundles *)
  let ( let* ) x f = Reader.bind f x in
  let* (MidiVal ctrl1) = MidiState.getControlR (MidiCh 0) (MidiCtrl 1) in
  let* (MidiVal ctrl2) = MidiState.getControlR (MidiCh 0) (MidiCtrl 2) in
  let* (MidiVal ctrl3) = MidiState.getControlR (MidiCh 0) (MidiCtrl 3) in
  let* (MidiVal ctrl4) = MidiState.getControlR (MidiCh 0) (MidiCtrl 4) in
  let* (MidiVal ctrl5) = MidiState.getControlR (MidiCh 0) (MidiCtrl 5) in
  let* (MidiVal ctrl6) = MidiState.getControlR (MidiCh 0) (MidiCtrl 6) in
  let* (MidiVal ctrl7) = MidiState.getControlR (MidiCh 0) (MidiCtrl 7) in
  let* pitch = MidiState.getPitchR in
  let* trigger = MidiState.boolFromNote in
  (* create trigger from note On *)
  let () =
    currentState :=
      { c1= ctrl1
      ; p= pitch
      ; c2= ctrl2
      ; c3= ctrl3
      ; c4= ctrl4
      ; c5= ctrl5
      ; c6= ctrl6
      ; c7= ctrl7 }
  in
  (* write state ref *)
  Reader.return trigger

(* a function to scale_down a phase increase *)
let scale_down y x = x |> float_of_int |> fun x' -> x' /. y

let sec2samp s = s |> seci |> fun x -> Samps x

let wlkr midiIn arr divider lens =
  let wlk = walk 0.0 (midiIn |> lens |> map (scale_down divider)) |> trunc in
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

(* relNotes are notes that have a delay, before they play *)
let mkScore delaySq pitchesSq duraSq =
  zipWith3
    (fun delay off dura ->
      RelNote
        ( Samps (seci delay)
        , transP off (mapOverDuration (fun _ -> seci (0.01 *. dura)) c3) ))
    delaySq pitchesSq (st duraSq)
  |> List.of_seq |> relToScore

let ofTrigger trig =
  let stt = ofRef currentState in
  let l1 = map (fun s -> s.c1) in
  let l2 = map (fun s -> s.c2) in
  let l3 = map (fun s -> s.c3) in
  let l4 = map (fun s -> s.c4) in
  let l5 = map (fun s -> s.c5) in
  let l6 = map (fun s -> s.c6) in
  let l7 = map (fun s -> s.c7) in
  let entryDelays =
    stt |> l4 |> map (fun s -> s |> float_of_int |> ( *. ) (0.01 /. 4.))
  in
  let combinedWalks =
    [ wlkr stt [|5; 4; 7|] 16.0 l1
    ; wlkr stt [|5; 7; 4|] 16.0 l2
    ; wlkr stt [|12; 7; 0|] 16.0 l3 ]
    |> List.to_seq |> transpose |> concat
  in
  let direction = wlkr stt [|-1; 1|] 16.0 l5 in
  let arpSq =
    combinedWalks
    |> batcher (stt |> l6 |> ( +~ ) (st 1))
    |> map (fun steps -> walki 0 (direction *~ steps) |> ( +~ ) (stt |> l6))
    |> zipWith3
         (fun entry duraSq score -> mkScore (st entry) score duraSq)
         entryDelays
         (stt |> l7 |> floatify)
    |> map Option.some
  in
  weavePattern trig arpSq (st None)

(* this maps midi input msg to an output msg (raw midi) *)
let midiInputTestFun input =
  input |> MidiState.makeSeq (* take msg, make it a state *)
  |> map (Reader.run midiParser)
  (* run a bunch of readers to extract properties *)
  |> ofTrigger
  |> playArp |> serializeBundles |> map toRaw

let () = Midi.playMidi midiInputTestFun sr
