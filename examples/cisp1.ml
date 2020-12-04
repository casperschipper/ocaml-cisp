open Cisp
open Midi
open Seq


(* simple mod of controller 1 onto pitch *)

let sr = ref 44100.0

type data =
  { c1 : int; p : pitch ; c2 : int  }
let currentState = ref ({ c1 = 0 ; p = Pitch 0 ; c2 = 0 })
       
let pitchControl3 =
  let (let* ) x f = Reader.bind f x in
  let* (MidiVal ctrl1) = MidiState.getControlR (MidiCh 0) (MidiCtrl 1) in
  let* (MidiVal ctrl2) = MidiState.getControlR (MidiCh 0) (MidiCtrl 2) in
  let* pitch = MidiState.getPitchR in

  let* trigger = MidiState.boolFromNote in (* create trigger from note On *)
  
  let () = currentState := { c1 = ctrl1 ; p = pitch ; c2 = ctrl2 } in
  (* write state ref *)
  Reader.return ( trigger )

let ofTrigger trig =
  let midiIn = ofRef currentState in
  
  let myWalk = walki 0 (midiIn |> map (fun state -> state.c1)) in
  let arr = [|0;2;4;3|] in
  let ixi = index arr myWalk in

  let myWalk2 = walki 0 (midiIn |> map (fun state -> state.c2)) in
  let arr2 = [|-12;0;12;0;7;0;24|] in
              let ixi2 = index arr2 myWalk2 in
              

  (* this will not work, since the pitch is not updated all the time *)
  let pitch =
    midiIn |> map (fun state -> state.p)
  in
    

  let notes =  zipToNoteEvt
                 (MidiCh 1 |> st)
                 (zipWith transpose pitch (ixi +~ ixi2))
                 (Velo 100 |> st)
                 (Samps 1000 |> st)
  in
  weavePattern trig notes (st SilenceEvent)
  
 

(* this maps midi input msg to an output msg (raw midi) *)
let midiInputTestFun input =
  input
  |> MidiState.makeSeq (* take msg, make it a state *)
  |> map (Reader.run pitchControl3) (* run a bunch of readers to extract properties *)
  |> ofTrigger
  |> serialize |> map toRaw (* turn back into raw midi *)
             
  
let () = Midi.playMidi midiInputTestFun sr 
           
