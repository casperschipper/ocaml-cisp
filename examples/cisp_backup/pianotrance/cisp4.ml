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


  

let ofTrigger trig =
  (*let stt = ofRef currentState in
    let l1 s = s.c1 in
      let l2 s = s.c2 in
      let l3 s = s.c3 in
      let l5 s = s.c5 in
      let l6 s = s.c6 in*)
  (*let noteDurs =
      stt |> map (fun s -> s.c4 |> float_of_int |> ( *. ) (0.01 /. 4.) |> seci)
    in*)
  let noteDurs = seq [100;200;100;30000] *~ (st 4) in
  let patts =
    Array.map List.to_seq
      [| [100; 20; 100; 100; 50]
       ; [100; 50; 50; 50; 80]
       ; [100; 20; 50; 10; 40]
       ; [100; 40; 40]
       ; [100; 40; 100; 40]
       ; [100; 70; 60; 40]
       ; [100; 80; 40; 80; 90] |]
  in
  let arr = [|0; 1; 2|] in
  let idx () = count |> map (fun x -> x mod Array.length arr) in
  let value = map2 rvi (st 0) (st (Array.length patts)) in
  let clock = metre (st 30) in
  let writeHead = syncOverClock clock (zip value (idx ())) in
  let mutantCtrl = makeMutateArray (idx ()) writeHead in
  let mutant = mutateArrayi mutantCtrl arr in
  let indexed = index patts mutant in
  let catted = indexed |> concat in
  let pitches = seq [7;12;19;12;7;0;7;12] +~ (hold (st 6) (seq [0;7;4;2;7;0;9])) in
  let notesSq = (st makeNoteOfInts) <*> (pitches +~ (st 60)) <*> catted <*> noteDurs <*> (st 4) in
  weavePattern trig notesSq (st SilenceEvent)

(* this maps midi input msg to an output msg (raw midi) *)
let midiInputTestFun input =
  input |> MidiState.makeSeq (* take msg, make it a state *)
  |> map (Reader.run pitchControl3)
  (* run a bunch of readers to extract properties *)
  |> ofTrigger
  |> serialize |> map toRaw

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
