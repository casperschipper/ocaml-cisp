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
    seq [4;3;3;4] |> map (fun n () -> Cons (true, repeat n false)) |> concat
  in
  let defaultEvents = (* this is the prototype *) 
    st (NoteEvent (MidiCh 1, Pitch 60, Velo 100, seconds 0.2))
  in
  let pitchPattern =
   boundedWalk 82 ( [st (-7);st (-7);st (-7);st (-7);rv (st 0) (st 14)] |> Cisp.ofList |> transpose |> concat ) (fun x -> if x < 40 then 80 else if x > 120 then 80 else x)
  in
  trigger
  |> (fun pattern -> weavePattern pattern defaultEvents (st SilenceEvent))
  |> (fun p -> weavePattern mask p (st SilenceEvent))
  |> overwritePitch pitchPattern
  |> withDur (ch [|42050|])
  |> withChan (st 4)
  |> withVelo (st 100)
  |> serialize |> map toRaw


let () = Midi.playMidi midiInputTestFun sr
