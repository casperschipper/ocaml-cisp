open Cisp
open Midi
let euclidTrigger = Euclid.euclidTrigger 
    
let intervArr = Seq.unfold (fun state ->
                    if state > 90 then
                      None
                    else
                      Some (state, state + 12)) 40 |> Array.of_seq

let copy = Array.copy intervArr

let maxidx = Array.length intervArr 

let writer =
  let idx = rv (st 0) (st maxidx) in
  let f old original =
    let nw = old + (pickOne [|-7;7|]) in
    pickOne [|nw;original|] 
  in
  idx |> Seq.map (fun i -> intervArr.(i) <- f intervArr.(i) copy.(i)) |> syncEffectClock (interval (st 1)) 
  

let midiReader =
  let ( let* ) x f = Reader.bind f x in
  let* trigger = MidiState.boolFromNote in
  Reader.return trigger

let simpleWalk =
  let ( +~- ) a b = Seq.map ( ( + ) a ) b in 
  rv (st 0) (st 10) |> Seq.map (fun off -> off +~- seq [0;1;0;2]) |> concat |>  Seq.map (wrap 0 maxidx)
  (*
  
  let loopy = seq [0;3;6;9] |> Seq.map (fun x -> x +~- (seq [0;1;0;2]))  in
  let holdn = seq (shuffle [11;7;5]) in
  let offset = walki 2 (ch [|(-1);1|]) |> Seq.map (wrap 0 maxidx) |> hold holdn in
  let zipper l off = l +~ (st off |> take 3) in
  zipWith zipper loopy offset |> concat *)


let oneWalk = 
  let idx = simpleWalk in
  index intervArr idx




(*type walkmind direction is state *)
(* a walk that updates boundaries only when hit *)

let map = Seq.map
 
let notes channel =
  let shortdur =
    st 0.1
  in
  let longdur =
    seq [0.9;0.4;0.2;1.4]
  in
  let durs =
    [shortdur |> hold (seq [3;5;7])
    ;longdur |> hold (st 1)] |> List.to_seq |> transpose |> concat
  in
  st makeNoteOfInts
  <*> (oneWalk)
  <*> (seq [100;90;50;80;50;75;40])
  <*> (durs |> Seq.map seci)
  <*> (st channel)

let ofTrigger channel trig =
  weavePattern trig (map Option.some (notes channel)) (st None)

(* this clock only produces integer on the trigger 
t f f f t f f f t f f f 
[Some 0;None;None;None;None;Some 1;None;None;None;None;Some 2]
 *)
let countClock clickTrack =
   weavePattern clickTrack (Seq.map Option.some count) (st None)
    
let withDefault default opt =
  match opt with
  | Some x -> x
  | None -> default

(* this takes an optSq and a test (f), returns false unless test is true 
   interesting in combination with testClock
*)
let mapOverOpt f optSq =
  let g x = x |> Option.map f |> withDefault false in
  optSq |> Seq.map g

(* checks if equal *)
let modPulse n optSq =
  let f x = x mod n = 0 in
  mapOverOpt f optSq


let makeBundles (trigSq : bool Seq.t ) =
  let ns = [(7,12,1);(9,16,2);(5,6,3)] in
  let aSq (num,div,chan) =
    trigSq 
    |> (fun t -> weavePattern t (euclidTrigger num div) (st false))
    |> ofTrigger chan
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
  |> effectSync writer
  |> serializeBundles
  |> map toRaw


let () = Midi.playMidi midiFun Process.sample_rate 
