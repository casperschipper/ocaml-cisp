open Midi
open Cisp

type chord = Chord of int list

let mkChord lst = Chord lst

let getChord (Chord l) = l

let mutate f_one lst =
  let l = List.length lst in
  let idx = Random.int l in
  let new_value = f_one (List.nth lst idx) in
  Toolkit.update_nth lst idx new_value

type direction = 
  | Up 
  | Down
  | AnyDir

let simple_walk direction x = 
  let choice = Toolkit.choice (7) [4] in
  match direction with 
  | Up -> choice + x
  | Down -> choice - x
  | AnyDir -> Toolkit.choice (-1) [1] * choice + x 

let direction_of_x low high x = 
  if x < low then
    Up 
  else if x > high then 
    Down 
else 
  AnyDir

let rec transpose_in_range low high x =
  if low > high then
    failwith
      ( "transpose in range, but low is higher than high" ^ string_of_int low
      ^ string_of_int high )
  else if high - low < 12 then
    failwith
      ("too small range to transpose" ^ string_of_int low ^ string_of_int high)
  else if x < low then transpose_in_range low high (x + 12)
  else if x > high then transpose_in_range low high (x - 12)
  else x

let no_doubles lst =
  let n = List.length lst in
  let m = List.length (List.sort_uniq compare lst) in
  n = m

let try_until_no_doubles lst f =
  let rec aux result = if no_doubles result then result else aux result in
  aux (f lst)

let combi chord =
  chord |> mutate (fun p -> p |> simple_walk (direction_of_x 30 90 p) )

let (stream_of_chords : midiBundle Seq.t) =
  let update old_chord =
    let new_chord = try_until_no_doubles old_chord combi |> List.sort compare in
    (new_chord, new_chord)
  in
  Infseq.unfold update [60; 67;72;79]
  |> Infseq.map List.to_seq |> Infseq.toSeq
  |> Seq.map (Seq.map (fun p -> mkNoteClip 1 p 100 8000))
  |> Seq.map chord

let vis inp =
  let stream =
    mkNoteClip <$> st 1
    <*> Seq.map2 ( + ) (Cisp.seq [0; 4; 7]) (st 60)
    <*> seq [100; 80; 40; 80]
    <*> seq [1000; 200; 20]
  in
  Midi.trigger stream inp |> serialize |> Seq.map toRaw

let cacheSeq str n =
  str |> Seq.take n |> List.of_seq |> Cisp.seq


let f2 inp =
  let seqqq =
    cacheSeq stream_of_chords 1
  in
  inp |> Seq.map isNoteOn
  |> Midi.triggerBundle seqqq
  |> serializeBundles |> Seq.map toRaw


let () =
  let midiThread () =
    Midi.playMidi f2 Process.sample_rate ;
    while true do
      Unix.sleep 60
    done
  in
  let _ = Thread.create midiThread () in
  let _ =
    Unix.sleep 4;
    print_int
      (Sys.command
         "jack_connect ocaml_midi:ocaml_midi_out system_midi:playback_1" ) ;
    print_int
      (Sys.command "jack_connect system_midi:capture_2 ocaml_midi:ocaml_midi_in") ;
    ignore (Sys.command "jack_lsp -c -A | grep ocaml")
  in
  while true do
    Unix.sleep 30
  done
