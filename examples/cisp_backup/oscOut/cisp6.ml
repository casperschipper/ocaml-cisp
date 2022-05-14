open Cisp
open Midi
open Seq

let sr = ref 44100.0

(* this maps midi input msg to an output msg (raw midi) *)
let midiInputTestFun input =
  (* this translates input into boolean trigger *)
  (* a metre boolean mask that triggers notes or rests in the pattern *)
  (* dummy event *)
  (* apply metre to default event, use Silence as filler *)

  let trigger =
    map isNoteOn input (* this checks if it is a note pulse *) 
  in
  let mask =
    st true
  in
  let defaultEvents = (* this is the prototype *) 
    st (NoteEvent (MidiCh 1, Pitch 60, Velo 100, seconds 0.2))
  in
  let pitchPattern =
   seq [72;67;60;67] |> hold  (ch ([|1;1;1;1;1;3|] ) |> loop (st 2) (st 3) |> concat)
  in
  trigger
  |> (fun pattern -> weavePattern pattern defaultEvents (st SilenceEvent))
  |> (fun p -> weavePattern mask p (st SilenceEvent))
  |> overwritePitch pitchPattern
  |> withDur (ch [|4205;8000;8000;8000|])
  |> withChan (st 6)
  |> withVelo ([ line (seq [100.0;128.0]) (st 7.); line (seq [100.0;128.0]) (st 5.) ] |> ofList |> transpose |> concat |> trunc)
  |> serialize |> map toRaw


let () = Midi.playMidi midiInputTestFun sr
