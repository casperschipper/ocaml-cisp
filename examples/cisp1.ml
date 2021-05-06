open Cisp
open Midi
    
let intervArr = [|40;45;50;55;60;65;70;65;60;55;50;45|]
let copy = Array.copy intervArr

let maxidx = Array.length intervArr 

let writer =
  let idx = rv (st 0) (st maxidx) in
  let f old original =
    let nw = old + (pickOne [|-7;7|]) in
    pickOne [|nw;original|] 
  in
  idx |> Seq.map (fun i -> intervArr.(i) <- f intervArr.(i) copy.(i)) |> syncEffectClock (interval (st 10))
  

let midiReader =
  let ( let* ) x f = Reader.bind f x in
  let* trigger = MidiState.boolFromNote in
  Reader.return trigger

let simpleWalk =
  let loopy = seq [2;3] |> hold (seq [11;5;7]) |> Seq.map countTill in
  let holdn = seq (shuffle [11;7;5]) in
  let offset = walki 2 (ch [|(-1);1|]) |> Seq.map (wrap 0 maxidx) |> hold holdn in
  let zipper l off = l +~ (st off |> take 3) in
  zipWith zipper loopy offset |> concat 


let oneWalk = 
  let idx = simpleWalk in
  index intervArr idx



(*type walkmind direction is state *)
(* a walk that updates boundaries only when hit *)

let bpm = 170.0

let hz = (bpm /. 60.0) *. 4.0
 
let notes =
  st makeNoteOfInts
  <*> (effectSync writer oneWalk)
  <*> (st 80)
  <*> (st (seci (1.0 /. hz)))
  <*> (st 3)

let ofTrigger trig =
  weavePattern trig notes (st SilenceEvent)

let midiFun input =
  let map = Seq.map in
  input
  |> MidiState.makeSeq
  |> map (Reader.run midiReader)
  |> pulseDivider (st 0)
  |> ofTrigger
  |> serialize |> map toRaw


let () = Midi.playMidi midiFun Process.sample_rate 
