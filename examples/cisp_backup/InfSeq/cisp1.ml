open Cisp
open Midi

let ofOpt optEvt = match optEvt with Some evt -> evt | None -> SilenceEvent

let intervArr = [|7;4;4;7;3|] 

let writer =
  let index = wrappedCount intervArr in
  let generator = ch [|7;4;3;5|] in
  let wrf (i, v) = writeOne intervArr i v in
  zip index generator |> Seq.map wrf |> syncEffectClock (interval (rv (st 1) (st 10)))

let oneWalk start =
  let idx = wrappedCount intervArr in
  let steps = (index intervArr idx |> take 5) *~ (st (-1)) in
  walki start steps

let manyWalks =
  (st 76) |> Seq.map oneWalk |> concat

let pitchSq =
  effect writer manyWalks

let notes =
  st makeNoteOfInts
  <*> pitchSq
  <*> (st 80)
  <*> (st (seci 1.0))
  <*> (st 1)

let midiFun _ =
  let map = Seq.map in
  notes
  |> syncOverClock (clockGen (seq [3.0;4.0]))
  |> map ofOpt |> serialize |> map toRaw


let () = Midi.playMidi midiFun Process.sample_rate
