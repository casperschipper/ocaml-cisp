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
    seq [3;3;3;3;3;3;3;3;6] |> map (fun n () -> Cons (true, repeat n false)) |> concat
  in
  let defaultEvents = (* this is the prototype *) 
    st (NoteEvent (MidiCh 1, Pitch 60, Velo 100, seconds 0.2))
  in
  let pitchPattern =
    boundedWalk 48 ( [ch [|7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;4|]] |> Cisp.ofList |> transpose |> concat ) (fun x -> if x < 40 then 48 else if x > 80 then 48 else x)
  in
  trigger
  |> (fun pattern -> weavePattern pattern defaultEvents (st SilenceEvent))
  |> (fun p -> weavePattern mask p (st SilenceEvent))
  |> overwritePitch pitchPattern
  |> withDur (trunc <| line (seq [1000.0;100000.0]) (st 30.0))
  |> withChan (st 7)
  |> withVelo (st 110)
  |> serialize |> map toRaw


let () = Midi.playMidi midiInputTestFun sr
