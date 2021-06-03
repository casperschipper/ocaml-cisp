open Cisp
open Midi

let map = Seq.map

let midiReader =
  let ( let* ) x f = Reader.bind f x in
  let* trigger = MidiState.boolFromNote in
  Reader.return trigger

let rhythm () =
  let (a,b) = (rvi 3 6,rvi 1 2) in
  let t = true in
  let f = false in
  seq [t;f] |> hold (seq [a;b])
  
let onePitchLoop () =
  let n = rvi 2 11 in
  let steps = [|-7;7;0;0;2;-2|] in
  let pitchStack = walki 0 (ch steps) |> take n |> List.of_seq in                
  let melody = index pitchStack (seq [0;1;1;0;0;1;2;1;0]) in
  melody

               
  
let notes channel =
  st makeNoteOfInts 
  <*> (onePitchLoop () +~ (st (36 + (channel * 7))))
  <*> (st 80)
  <*> (seci 0.4 |> st)
  <*> (st channel)

let ofTrigger trig channel =
  let s = syncOverClock (rhythm ()) (notes channel) in
  weavePattern trig s (st None)

let mkBundles t =
  let addOptToBundle opt bundle =
    match opt with
    | Some evt -> addToBundle bundle evt
    | None -> bundle
  in
  let chs = [1;2;3;4;5;6;7] in
  chs |> List.map (fun channel -> ofTrigger t channel) |>  list_fold_heads_with silenceBundle addOptToBundle
  

  
let midiFun input =
  input
  |> MidiState.makeSeq
  |> map (Reader.run midiReader)
  |> mkBundles
  |> serializeBundles
  |> map toRaw
       
let () = Midi.playMidi midiFun Process.sample_rate    
 
