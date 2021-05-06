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

let map = Seq.map
 
let notes channel =
  st makeNoteOfInts
  <*> (oneWalk)
  <*> (st 80)
  <*> (st (seci (0.1)))
  <*> (st channel)

let ofTrigger channel trig =
  weavePattern trig (map Option.some (notes channel)) (st None)


let countClock clickTrack =
  recursive
    clickTrack
    (0,true)
    (fun click (state,_) -> if click then (state + 1,true) else (state,false))
    (fun tup -> match tup with
                  (state,true) -> Some state
                | (_,false) -> None)
    
    

let makeBundles (trigSq : bool Seq.t ) =
  let cnt = trigSq |> countClock in
  let ns = [1;2;3] in
  let aSq n =
    cnt
    |> map (fun x -> match x with Some x -> x mod n = 0 | None -> false)
    |> ofTrigger 1
  in
  let addOptToBundle opt bundle =
    match opt with
    | Some evt -> addToBundle bundle evt
    | None -> bundle
  in
  ns |> List.map aSq |> list_fold_heads_with silenceBundle addOptToBundle
  

let midiFun input =
  input
  |> MidiState.makeSeq
  |> map (Reader.run midiReader)
  |> makeBundles
  |> serializeBundles |> map toRaw


let () = Midi.playMidi midiFun Process.sample_rate 
