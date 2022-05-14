open Cisp
open Midi


let simpleWalk  =
  wander 0
    (seq [6;0;6;0;6;1;5;2;4;3;4;6;4;6;5;6;0] |> Infseq.cycleSq) |> Infseq.toSeq
    
let intervArr = [|60;60;60;60;60;60|]

let writer =
  let index = wrappedCount intervArr in
  let generator = walki 74 ((st (-1)) *~ ch [|7;7;4;5;-7;4;12;-12|]) |> take 14 |> st |> concat in
  let wrf (i,v) = writeOne intervArr i v in
  zip index generator |> Seq.map wrf |> syncEffectClock (interval (seq [4;5;3;6;7;3;3;5;11;17]))


let oneWalk = 
  let idx = simpleWalk in
  index intervArr idx |> take 30
  

(*type walkmind direction is state *)
(* a walk that updates boundaries only when hit *)



let simplePitch =
  effectSync writer (st oneWalk |> concat)

let ofOpt optEvt = match optEvt with Some evt -> evt | None -> SilenceEvent

let bpm = 100.0

let hz = (bpm /. 60.0) *. 4.0

let notes =
  st makeNoteOfInts
  <*> simplePitch
  <*> (st 80)
  <*> (st (seci (1.0 /. hz)) *~ (seq [1;1;2;1;1;4]))
  <*> (st 1)

let midiFun _ =
  let map = Seq.map in
  notes
  |> syncOverClock (clockGen (seq [hz]) )
  |> map ofOpt |> serialize |> map toRaw


let () = Midi.playMidi midiFun Process.sample_rate 
