open Cisp
open Midi


let simpleWalk  =
  wander 0
    (seq [6;1;5;2;4;3;4;6;4;6;5;6;0] |> Infseq.cycleSq) |> Infseq.toSeq
    
let intervArr = [|7;4;4;7;3;7|]

let writer =
  let index = wrappedCount intervArr in
  let generator = ch [|7;4;3;5|] in
  let wrf (i,v) = writeOne intervArr i v in
  zip index generator |> Seq.map wrf |> syncEffectClock (interval (lift rv 1 10))


let oneWalk start = 
  let idx = simpleWalk in
  let steps = (index intervArr idx |> take 25) *~ (st (-1)) in
  walki start steps

(*type walkmind direction is state *)
(* a walk that updates boundaries only when hit *)


let manyWalks =
  (st 76) |> Seq.map oneWalk |> concat


let simplePitch =
  effectSync writer manyWalks 

let ofOpt optEvt = match optEvt with Some evt -> evt | None -> SilenceEvent


let notes =
  st makeNoteOfInts
  <*> simplePitch
  <*> (st 80)
  <*> (st (seci 0.1))
  <*> (st 1)

let midiFun _ =
  let map = Seq.map in
  notes
  |> syncOverClock (clockGen (seq [9.0]) )
  |> map ofOpt |> serialize |> map toRaw


let () = Midi.playMidi midiFun Process.sample_rate 
