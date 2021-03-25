open Cisp
open Midi
open Seq

(* simple mod of controller 1 onto pitch *)

let sr = ref 44100.0

type data = {c1: int; p: pitch; c2: int; c3: int; c4: int; c5: int; c6: int}

let currentState = ref {c1= 0; p= Pitch 0; c2= 0; c3= 0; c4= 1; c5= 1; c6= 1}

let pitchControl3 =
  let ( let* ) x f = Reader.bind f x in
  let* (MidiVal ctrl1) = MidiState.getControlR (MidiCh 0) (MidiCtrl 1) in
  let* (MidiVal ctrl2) = MidiState.getControlR (MidiCh 0) (MidiCtrl 2) in
  let* (MidiVal ctrl3) = MidiState.getControlR (MidiCh 0) (MidiCtrl 3) in
  let* (MidiVal ctrl4) = MidiState.getControlR (MidiCh 0) (MidiCtrl 4) in
  let* (MidiVal ctrl5) = MidiState.getControlR (MidiCh 0) (MidiCtrl 5) in
  let* (MidiVal ctrl6) = MidiState.getControlR (MidiCh 0) (MidiCtrl 6) in
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
      ; c6= ctrl6 }
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
           (Samps delay, transP off (mapOverDuration (fun _ -> seci 0.1) c3)))
       delaySq
  |> List.of_seq |> relToScore

(** everything needs to be a stream, this is too static *)
let ofTrigger trig =
  let stt = ofRef currentState in
  let l1 s = s.c1 in
  let l2 s = s.c2 in
  let l3 s = s.c3 in
  let l5 s = s.c5 in
  let l6 s = s.c6 in
  let entryDelays =
    stt |> map (fun s -> s.c4 |> float_of_int |> ( *. ) (0.01 /. 4.))
  in
  let combinedWalks =
    wlkr stt [|0; 7; 14; 21; 28|] 4.0 l1
    +~ wlkr stt [|0; 5; 10; 7; 12; 17|] 4.0 l2
    +~ wlkr stt [|-12; 0; 12; 0; -12|] 4.0 l3
    +~ (stt |> map l5 |> map (fun x -> (x * 5) - 60))
  in
  let compare x y = if x < y then 1 else -1 in
  let arpSq =
    combinedWalks
    |> batcher (stt |> map l6 |> ( +~ ) (st 1))
    |> map (fun sq -> sq |> List.of_seq |> List.sort compare |> List.to_seq)
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
