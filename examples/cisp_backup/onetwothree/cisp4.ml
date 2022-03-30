open Cisp
open Midi

let map = Seq.map

let midiReader =
  MidiState.boolFromNote

(*
    .
    1 2 3 4

    1 3 4
      .
    
    1 4
    .
    1
    .
    1
    .
    1 2 
      .
    1 3
    .
    1 4
      .
 *)


let notes channel =
  st makeNoteOfInts 
  <*> (st 60)
  <*> (st 60)
  <*> (seci 0.1 |> st)
  <*> (st 0)

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

   

    
    


  
  
            
 
