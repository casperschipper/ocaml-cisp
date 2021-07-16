open Cisp
open Midi

let map = Seq.map

let midiReader =
  let ( let* ) x f = Reader.bind f x in
  let* trigger = MidiState.boolFromNote in
  Reader.return trigger

let from01 x =
  match x with
    1 -> true | _ -> false
  
let rhythm () =
  let ps = lift rv 1 15 |> take 3 in
  let mapped = List.of_seq ps |> List.map toBinary |> List.concat in
  seq mapped |> map from01
    
  
let onePitchLoop () =
  let a = rvi 36 52 in
  let b = rvi 36 52 in
  let ps = lift rv 1 15 |> take 3 in
  let mapped = List.of_seq ps |> List.map toBinary |> List.concat in
  seq mapped |> index [|a;b|]

let velo () =
  let a = 50 in
  let b = 80 in
  let ps = lift rv 1 15 |> take 3 in
  let mapped = List.of_seq ps |> List.map toBinary |> List.concat in
  seq mapped |> index [|a;b|] 
          
let notes channel =
  st makeNoteOfInts 
  <*> onePitchLoop ()
  <*> velo ()
  <*> (seci 0.1 |> st)
  <*> (st channel)

(* syncOverClock *)
let ofTrigger trig channel =
  let p = pickOne [|2;3|] in
  let s = syncOverClock (rhythm () |> pulseDivider (st p)) (notes channel) in
  weavePattern trig s (st None)

let mkBundles t =
  let addOptToBundle opt bundle =
    match opt with
    | Some evt -> addToBundle bundle evt
    | None -> bundle
  in
  let chs = rangei 1 10 |> List.of_seq in
  chs |> List.map (fun channel -> ofTrigger t channel) |>  list_fold_heads_with silenceBundle addOptToBundle
  

  
let midiFun input =
  input
  |> MidiState.makeSeq
  |> map (Reader.run midiReader)
  |> mkBundles
  |> serializeBundles
  |> map toRaw
       
  
let () =
  let f () =
    Midi.playMidi midiFun Process.sample_rate
    ; while true
      do
        Unix.sleep 60
      done
  in
  let _ = Thread.create f () in
  let _ = Sys.command "jack_disconnect system_midi:capture_2 ocaml_midi:ocaml_midi_in" in
  let _ = Sys.command "jack_disconnect ocaml_midi:ocaml_midi_out system_midi:playback_1" in
  let _ = Sys.command "jack_connect ocaml_midi:ocaml_midi_out system_midi:playback_5" in
  let _ = Sys.command "jack_connect system_midi:capture_1 ocaml_midi:ocaml_midi_in" in
  while true
  do
    Unix.sleep 60
  done

   

    
    


  
  
            
 
