open Cisp
open Midi
open Seq

(* this maps midi input msg to an output msg (raw midi) *)
let midiInputTestFun input =
  (* this translates input into boolean trigger *)
  let secs s = s * 44100 in
  let inputTrigger =
    map
      (fun midiMsg ->
        match midiMsg with NoteOn (_, _, _) -> true | _ -> false)
      input
  in
  (* a metre boolean mask that triggers notes or rests in the pattern *)
  let pitchPattern =
    boundedWalk 40
      (transpose ([st 2; ch [|2; 2; 2; 2; 2; 1|]; st 14] |> ofList) |> concat)
      (fun x -> if x < 40 then 40 else if x > 110 then 40 else x)
  in
  let reversePitch = map (fun x -> 100 - x) pitchPattern in
  (* dummy event *)
  let defaultEvts =
    st (NoteEvent (MidiCh 1, Pitch 60, Velo 100, seconds 0.2))
  in
  (* apply metre to default event, use Silence as filler *)
  inputTrigger
  |> (fun pattern -> weavePattern pattern defaultEvts (st SilenceEvent))
  |> withPitch reversePitch
  |> withDur (seq (List.map secs [1; 2; 4]))
  |> withChan (st 4)
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
