open Cisp
open Midi
let euclidTrigger = Euclid.euclidTrigger

 
let map = Seq.map 

(* extract properties from input *)
let midiReader =
  let ( let* ) x f = Reader.bind f x in
  let* trigger = MidiState.boolFromNote in
  Reader.return trigger

let lookup x =
  match x with 0 -> (-1) | 1 -> 1 | _ -> 0
    

let notes channel nt veloArr =
  let arrMax = Array.length veloArr in
  let velo =
    Euclid.euclid01 5 8 |> seq |> map lookup |> walki 0 |> map (wrap 0 arrMax) |> (index veloArr)
  in
  st makeNoteOfInts
  <*> (st nt)
  <*> (velo |> hold (seq [2;1;1;1;1;1]))
  <*> (0.1 |> seci |> st)
  <*> (st channel)

let ofTrigger channel nt arr trig =
  weavePattern trig (map Option.some (notes channel nt arr)) (st None)

let makeBundles (trigSq : bool Seq.t ) =
  let ns = [(7,12,1,36, [|100;80;20;70;60;40;100|] );
            (5,9,2,38, [|40;100|])
            ;(9,13,3,39, [|100;80|])
           ;(1,1,2,42, [|60;80;110|])] in
  let aSq (num,div,chan,nt,arr) =
    trigSq 
    |> (fun t -> weavePattern t (euclidTrigger num div) (st false))
    |> ofTrigger chan nt arr
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
  |> serializeBundles
  |> map toRaw


let () = Midi.playMidi midiFun Process.sample_rate 
