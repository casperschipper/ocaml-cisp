open Cisp
open Midi
open Seq

let sr = ref 44100.0


(* this maps midi input msg to an output msg (raw midi) *)
let midiInputTestFun input =
  (* this translates input into boolean trigger *)
  let inputTrigger = map isNoteOn input in
  (* a metre boolean mask that triggers notes or rests in the pattern *)
  let pitchPattern =
    boundedWalk 60
      ( seq [0; 0; 7;-7]
      |> hold (seq [2; 1] |> hold (seq [3; 5; 1])) )
      (fun x -> if x < 40 then 50 else if x > 120 then 70 else x)
  in
  (* dummy event *)
  let defaultEvts =
    st (NoteEvent (MidiCh 2, Pitch 60, Velo 100, seconds 0.2))
  in
  (* apply metre to default event, use Silence as filler *)
  inputTrigger
  |> (fun pattern -> weavePattern pattern defaultEvts (st SilenceEvent))
  |> withPitch pitchPattern
  |> withDur (ch [|4250|])
  |> withVelo (st 100)
  |> withChan (st 2)
  |> serialize |> map toRaw


let () = Midi.playMidi midiInputTestFun sr
