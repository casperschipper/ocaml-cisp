open Cisp
open Midi
open Seq

(* this maps midi input msg to an output msg (raw midi) *)
let sq input =
  (* this translates input into boolean trigger *)
  let inputTrigger =
    map
      (fun midiMsg ->
        match midiMsg with NoteOn (_, _, _) -> true | _ -> false)
      input
  in
  (* a metre boolean mask that triggers notes or rests in the pattern *)
  let oneLine mupper =
    let f x = x *. mupper in
    line (seq [50.0; 100.0]) (seq (List.map f [10.0; 40.0; 30.0]))
    |> map Int.of_float
  in
  let metre = st true in
  let pitchPattern =
    List.to_seq
      [ boundedWalk 60
          (seq [0; 7; 7; -12] |> hold (seq [1; 1] |> hold (seq [1; 3; 5])))
          (fun x -> if x < 20 then 50 else if x > 110 then 70 else x)
      ; boundedWalk 60
          (seq [-3; -4; 7; 12] |> hold (seq [1; 1] |> hold (seq [2; 4; 2])))
          (fun x -> if x < 20 then 50 else if x > 110 then 70 else x) ]
    |> transpose |> concat
  in
  (* dummy event *)
  let defaultEvts =
    st (NoteEvent (MidiCh 1, Pitch 60, Velo 100, seconds 0.2))
  in
  (* apply metre to default event, use Silence as filler *)
  let maskedEvts = weavePattern metre defaultEvts (st SilenceEvent) in
  inputTrigger
  |> (fun pattern -> weavePattern pattern maskedEvts (st SilenceEvent))
  |> withPitch pitchPattern
  |> withDur
       ( ch [|4250; 9000; 12000; 1000|]
       |> loop (lift rv 1 10) (lift rv 1 10)
       |> concat )
  |> withChan (st 4)
  |> withVelo
       ( [oneLine 1.7; oneLine 0.3; oneLine 0.5]
       |> List.to_seq |> transpose |> concat )
  |> serialize |> map toRaw

let () = playMidi sq (ref 0.)
