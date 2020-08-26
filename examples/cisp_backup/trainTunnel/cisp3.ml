open Cisp
open Midi
open Seq

(* this maps midi input msg to an output msg (raw midi) *)
let midiInputTestFun input =
  (* this translates input into boolean trigger *)
  let inputTrigger =
    map
      (fun midiMsg ->
        match midiMsg with NoteOn (_, _, _) -> true | _ -> false)
      input
  in
  (* a metre boolean mask that triggers notes or rests in the pattern *)
  let metre =
    seq [7; 11] |> map (fun n () -> Cons (true, repeat n false)) |> concat
  in
  let pitchPattern =
    boundedWalk 80
      (seq [-7; 7] |> hold (seq [3; 5; 7]))
      (fun x -> if x > 110 then 30 else if x < 20 then 100 else x)
  in
  (* dummy event *)
  let defaultEvts =
    st (NoteEvent (MidiCh 1, Pitch 60, Velo 100, seconds 0.2))
  in
  (* apply metre to default event, use Silence
     as filler *)
  let maskedEvts = weavePattern metre defaultEvts (st SilenceEvent) in
  inputTrigger
  |> (fun pattern -> weavePattern pattern maskedEvts (st SilenceEvent))
  |> withPitch pitchPattern
  |> withDur (ch [|88050|])
  |> withChan (st 6)
  |> withVelo (st 100)
  |> serialize |> map toRaw

let run () =
  let state = justSilence in
  (* the sq state var *)
  let inputRef = ref MidiSilence in
  let () = state := ofRef inputRef |> midiInputTestFun in
  let callback input =
    (* Slightly troublesome, this looks like a pure function (a -> b) but reads and writes to references *)
    let out =
      match !state () with
      | Cons (curr, tl) ->
          let () = state := tl in
          curr
      | Nil -> (0, 0, 0)
    in
    let () = inputRef := fromRaw input in
    out
  in
  JackMidi.playMidi callback samplerate

let () = run ()
