open Cisp
open Midi

let map = Seq.map
let f opt = match opt with Some evt -> evt | None -> SilenceEvent

let pattern =
  let starts = Infseq.seq [ 60; 64; 67] |> Infseq.map float_of_int in
  let steps = Infseq.repeat (List.to_seq [ 0;4;0;7;12;7;4;7;0 ] |> Cisp.cyclen 2 |> floatify) in
  Cisp.many_walks starts steps |> Infseq.map int_of_float

let () = 
  pattern |> Infseq.take 10 |> Seq.iter (fun f -> f |> debugi "inty: ")

let monitor m =
    match m with
    | true -> print_endline "yo!";m
    | false -> ();m


let midiFun input =
  let p =
    pattern
    |> Infseq.map (fun p -> mkNoteClip 1 p 100 100)
    |> Infseq.toSeq
  in
  let synced = input |> Seq.map (fun x -> x |> Midi.isNoteOn |> monitor)|> fun t -> syncOverClock t p in
  let optf x = Seq.map f x in 
  synced |> optf |> serialize |> map toRaw

let () =
  let f () =
    Midi.playMidi midiFun Process.sample_rate;
    while true do
      Unix.sleep 60
    done
  in
  let _ = Thread.create f () in
  let _ =
    Unix.sleep 1;
    Sys.command "jack_connect ocaml_midi:ocaml_midi_out system_midi:playback_1"
  in
  let _ =
    Unix.sleep 1;
    Sys.command "jack_connect system_midi:capture_2 ocaml_midi:ocaml_midi_in"
  in
  while true do
    Unix.sleep 60
  done
