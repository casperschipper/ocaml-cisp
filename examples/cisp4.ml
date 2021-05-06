open Cisp
open Midi
    
let intervArr = [|50;53;55;57;60;62;64;67;69|]
let copy = Array.copy intervArr

let maxidx = Array.length intervArr 

let writer =
  let idx = rv (st 0) (st maxidx) in
  let f old original =
    let nw = old + (pickOne [|-2;2|]) in
    pickOne [|nw;original|] 
  in
  idx |> Seq.map (fun i -> intervArr.(i) <- f intervArr.(i) copy.(i)) |> syncEffectClock (interval (st 10))
  

let midiReader =
  let ( let* ) x f = Reader.bind f x in
  let* trigger = MidiState.boolFromNote in
  Reader.return trigger

let simpleWalk =
  let ( +~- ) a b = Seq.map ( ( + ) a ) b in 
  let loopy = seq [0;3;6;9] |> Seq.map (fun x -> x +~- (seq [0;1;0;2]))  in
  let holdn = seq (shuffle [11;7;5]) in
  let offset = walki 2 (ch [|(-1);1|]) |> Seq.map (wrap 0 maxidx) |> hold holdn in
  let zipper l off = l +~ (st off |> take 3) in
  zipWith zipper loopy offset |> concat 


let oneWalk = 
  let idx = simpleWalk in
  index intervArr idx




(*type walkmind direction is state *)
(* a walk that updates boundaries only when hit *)


 
let notes channel =
  st makeNoteOfInts
  <*> (effectSync writer oneWalk)
  <*> (st 80)
  <*> (st (seci (0.1)))
  <*> (st channel)

let ofTrigger channel trig =
  weavePattern trig (notes channel) (st SilenceEvent)

let map = Seq.map

  
let polyphoneBundlesSq (seq : midiEvent Seq.t Seq.t) =
  let soloBundle = 
    Seq.map (fun midiEvtSq -> Seq.map soloBundle midiEvtSq) seq
  in
  Seq.fold_left mergeBundlesSq (st emptyBundle) soloBundle

(*
let makeNotes n trigSq =
   trigSq 
   |> pulseDivider 
   |> (ofTrigger n)*)



let makeBundles (trigSq : bool Seq.t ) =
  let ns = [(3,0);(3,1)] |> List.to_seq in
  ns |> map (fun (n,off) ->
            trigSq
            |> pulseDivider (seq [0;0;0;2])
            |> ofTrigger 1)
            |> polyphoneBundlesSq

let midiFun input =
  input
  |> MidiState.makeSeq
  |> map (Reader.run midiReader)
  |> makeBundles
  |> serializeBundles |> map toRaw


let () = Midi.playMidi midiFun Process.sample_rate 
