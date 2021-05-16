open Cisp
open Midi

let euclidTrigger = Euclid.euclidTrigger
let encode = Euclid.encode
let map = Seq.map 

(* extract properties from input *)
let midiReader =
  let ( let* ) x f = Reader.bind f x in
  let* trigger = MidiState.boolFromNote in
  Reader.return trigger



let sequenced pat =
  let init = pat in
  let f state =
    match state () with
    | Seq.Cons((p,n),tl) ->
       if n <= 1 then
         Some (Some p,tl)
       else
         Some (None,fun () -> Seq.Cons((p,n-1),tl))
    | Seq.Nil -> None
  in
  Seq.unfold f init

let notesa =
  let pat1 = [(60,1);(64,2);(67,1);(72,2);(60,1)] |> seq |> hold (sometimes 1 2 10) in
  pat1 |> sequenced |> map (fun opt ->
                   match opt with
                   | None -> SilenceEvent
                   | Some p -> makeNoteOfInts p 100 (0.1 |> seci) 1)

let notesb  =
  let pat1 = [(67,2);(65,2);(67,1);(60,1);(60,1)] |> seq |> hold (pulse (seq [1;2]) (st 1) (st 2)) in
  pat1 |> sequenced |> map (fun opt ->
                   match opt with
                   | None -> SilenceEvent
                   | Some p -> makeNoteOfInts p 100 (0.1 |> seci) 2)
let notesc  =
  let pat1 = [(48,1);(48,3);(55,2);(60,1);(58,1);(60,1);(55,1)] |> seq in
  pat1 |> sequenced |> map (fun opt ->
                   match opt with
                   | None -> SilenceEvent
                   | Some p -> makeNoteOfInts p 100 (0.1 |> seci) 3)

let notesd  =
  let pitches = st (walki 84 (ch [|(-12);(12);(-7);(7)|]) |> take 10) |> concat
                |> Seq.map (fun x -> (walki x (ch [|(-7);5;(-7);(5);(5);(-7);(-7)|]) |> take 4)) |> concat in
  let times = seq [2;2;2;1;1;1;2;2;2;2;2;2;2;2;3;2;3;1;1] in
  let z = zip pitches times in
  let pat1 = z in
  pat1 |> sequenced |> map (fun opt ->
                   match opt with
                   | None -> SilenceEvent
                   | Some p -> makeNoteOfInts p 100 (0.1 |> seci) 4)

let ofTrigger trig nts =
  weavePattern trig (map Option.some nts) (st None)

let makeBundles (trigSq : bool Seq.t ) =
  let seqList = [ notesa ;notesb ; notesc; notesd ] |> List.map (fun ns -> ofTrigger trigSq ns) in
  let addOptToBundle opt bundle =
    match opt with
    | Some evt -> addToBundle bundle evt
    | None -> bundle
  in
  seqList |> list_fold_heads_with silenceBundle addOptToBundle
  

let midiFun input =
  input 
  |> MidiState.makeSeq
  |> map (Reader.run midiReader)
  |> makeBundles
  |> serializeBundles
  |> map toRaw


let () = Midi.playMidi midiFun Process.sample_rate 
