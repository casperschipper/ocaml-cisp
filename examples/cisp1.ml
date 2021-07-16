open Cisp
open Midi

let map = Seq.map

let midiReader =
  MidiState.triggerFromChannelR (mkChannelClip 1) 
(*

  let chan1 = mkChannelClip 1 in
  let chan2 = mkChannelClip 2 in 
  Reader.map2
    pair
    (MidiState.triggerFromChannelR chan1)
    (MidiState.getDepressedR chan2 |> Reader.map (List.map fst)) *)
  
  
let currentChord = ref [60;64;67]

type arpy =
  { ix : int
  ; value : int option }

let arpeggiator notesListSq =
  let f lst { ix; _ } =
    match List.nth_opt lst ix  with
    | Some v -> { value = Some v ; ix = ix + 1 }
    | None -> { value = List.nth_opt lst 0 ; ix = 0 }
  in
  let eval { value; _ }  =
    value
  in
  recursive notesListSq { ix = 0; value = None } f eval
 
let notes =
  st makeNoteOfInts 
  <*> (st 60) (* ((ofRef currentChord |> arpeggiator) |> map (Option.value ~default:60)) *)
  <*> (st 60) 
  <*> (seci 0.1 |> st)
  <*> (st 0)

let ofTrigger parsedMidi =
  (* let trigger =
    Seq.map (fun (trigger,_) ->    
    trigger) parsedMidi
  in*)
  weavePattern parsedMidi (st c4) (st SilenceEvent)





  
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

   

    
    


  
  
            
 
