open Cisp
open Midi

let map = Seq.map

let midiReader =
  MidiState.boolFromChannelR (mkChannelClip 1) 
    (* let chan1 = mkChannelClip 1 in *)
  (* 
  let chan2 = mkChannelClip 2 in
  let depreader = (MidiState.getDepressedR chan2 |> Reader.map (List.map fst)) in
  Reader.map2
    pair
    (MidiState.boolFromNote)
    depreader*)
  
  
let currentChord = ref [60;64;67]

type arpy =
  { ix : int
  ; value : int option }

let arpeggiator notesListSq =
  let f lst { ix; _ } =
    match List.nth_opt lst ix  with
    | Some v -> { value = Some v ; ix = ix + 1 }
    | None -> { value = List.nth_opt lst 0 ; ix = 1 }
  in
  let eval { value; _ }  =
    value
  in
  recursive1 notesListSq { ix = 0; value = None } f eval
 
let notes =
  st makeNoteOfInts 
  <*> (st 60) 
  <*> (st 60) 
  <*> (seci 0.1 |> st)
  <*> (st 0)

let ofTrigger trigger =
  weavePattern trigger notes (st SilenceEvent)


let midiFun input =
  input 
  |> MidiState.makeSeq
  |> map (Reader.run midiReader)
  |> ofTrigger 
  |> serialize 
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

   

    
    


  
  
            
 
