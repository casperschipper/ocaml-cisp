open Cisp
open Midi
open Seq

(* simple mod of controller 1 onto pitch *)

let sr = ref 44100.0

let channel = 1 

let latchControl midiInputSq =
  let update currentMidiMsg (_,myControl) =
    match currentMidiMsg with
    | Control (MidiCh 0,MidiCtrl _,MidiVal value) ->
       let () =
         print_string "hallo"; print_int value 
       in
      (MidiSilence, value)
    | anything ->
      (anything, myControl)
  in
  let evaluate =
    id
  in
  recursive 
    midiInputSq
    (MidiSilence,0)
    update
    evaluate

    
     
let fromControl ((event, control),extraPar) =
  match event with
  | NoteOn (_,_,_)->
     mkNoteClip 1 (0+control - ((control mod 12) + extraPar)) 100 3000
  | _ ->
     SilenceEvent

let fromControlSq sq =
  map fromControl sq


  
            
(* TODO recursive clock mapping *)            
(* this maps midi input msg to an output msg (raw midi) *)
let midiInputTestFun input =
  input
  |> trigger (st c3)
  |> serialize |> map toRaw
                         
let () = Midi.playMidi midiInputTestFun sr 
