open Cisp
open Midi
    
let intervArr = [|60;72;67;60;67;72|]

let writer =
  let index = wrappedCount intervArr in
  let generator = ch [|-7;-5;12;-12|] |> hold (ch [|1;2;3|]) |> walki 74 |> take 6 |> st |> concat  in
  let wrf (i,v) = writeOne intervArr i v in
  zip index generator |> Seq.map wrf |> syncEffectClock (interval (seq [5;7;11;7;13]))

let simpleWalk () = 
  wander 0 (seq [0;3;0;3;0;3;0;6;0;6;3] |> Infseq.cycleSq) |> Infseq.toSeq


let oneWalk n = 
  let idx = simpleWalk () in
  index intervArr idx |> take n



(*type walkmind direction is state *)
(* a walk that updates boundaries only when hit *)

let manyWalk =
  let many = seq [5;10;15] |> Seq.map oneWalk |> concat in
  many


let simplePitch =
  effectSync writer manyWalk

let ofOpt optEvt = match optEvt with Some evt -> evt | None -> SilenceEvent

let bpm = 70.0

let hz = (bpm /. 60.0) *. 4.0
 
let notes =
  st makeNoteOfInts
  <*> simplePitch
  <*> (st 80)
  <*> (st (seci (1.0 /. hz)) *~ (seq [2;1;5;1;1;9]))
  <*> (st 1)

let midiFun _ =
  let map = Seq.map in
  notes
  |> syncOverClock (clockGen (seq [hz *. 1.5;hz]))
  |> map ofOpt |> serialize |> map toRaw


let () = Midi.playMidi midiFun Process.sample_rate 
