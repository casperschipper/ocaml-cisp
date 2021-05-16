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
    Euclid.euclid01 5 8 |> seq |> hold (seq [2;3])  |> map lookup |> walki 0 |> map (wrap 0 arrMax) |> (index veloArr) 
  in
  let pitchArray = [0;12;0;12;0;7;2;0;2;0] in
  let pitch =
    seq pitchArray 
    |> hold (Euclid.euclid01 2 3 |> seq |> map (fun x -> match x with 0 -> 2 | _ -> 5 ))
  in
  st makeNoteOfInts
  <*> (st nt +~ pitch)
  <*> velo
  <*> (0.1 |> seci |> st)
  <*> (st channel)

let ofTrigger channel nt arr trig =
  weavePattern trig (map Option.some (notes channel nt arr)) (st None)

let makeBundles (trigSq : bool Seq.t ) =
  let ns = [(10,16,4,36, [|100;80;120;60|])] in
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
