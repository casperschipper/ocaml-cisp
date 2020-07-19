open Seq

external open_midi_stream :
     (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
  -> (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
  -> (int -> unit)
  -> (int -> unit)
  -> unit = "open_midi_stream"

let absentNote = [|0x00; 0x00; 0x00|]

(*
let printState arrRef nfram =
  for i = 0 to nfram do
    match !arrRef.(i) with
    | st, _, _ -> if st = 0x90 then print_string "note\n" else ()
  done*)
let printRaw (st, d1, d2) frame =
  if st != 0 || d1 != 0 || d2 != 0 then (
    let string =
      String.concat " " (List.map Int.to_string [st; d1; d2; frame]) ^ "\n"
    in
    print_string string ; print_newline () )
  else ()

type deserializerState = {buffer: (int * int * int) Array.t; wrIndex: int}

let emptyDS size = {buffer= Array.make size (0, 0, 0); wrIndex= 0}

let updateDS state m =
  let () = state.buffer.(state.wrIndex) <- m in
  {state with wrIndex= state.wrIndex + (1 mod Array.length state.buffer)}

let readIndex state =
  if state.wrIndex + 1 > Array.length state.buffer then 0 else state.wrIndex + 1

let toSeq state =
  let safe i = i mod Array.length state.buffer in
  let rec aux idx () =
    if idx > Array.length state.buffer then Nil
      (* outside of buffer is undefined *)
    else
      let curIdx = readIndex state + idx |> safe in
      Cons (state.buffer.(curIdx), aux (curIdx + 1 |> safe))
  in
  aux 0

let inArrayToSeq arr writeFrame =
  let size = Bigarray.Array1.dim arr in
  let rec aux offset () =
    let idx = (writeFrame + offset) * 3 in
    if idx > size then Nil
    else
      let msg = (arr.{idx}, arr.{idx + 1}, arr.{idx + 2}) in
      Cons (msg, aux (offset + 1))
  in
  aux 0

let playMidi midiOutSq midiInRef sample_rate =
  let open Seq in
  let ar_out =
    Bigarray.Array1.create Bigarray.Int8_unsigned Bigarray.c_layout 4096
  in
  let ar_in =
    Bigarray.Array1.create Bigarray.Int8_unsigned Bigarray.c_layout 4096
  in
  let state = ref midiOutSq in
  open_midi_stream ar_out ar_in
    (fun nframes ->
      for i = 0 to nframes - 1 do
        let midi_frame = i * 3 in
        midiInRef := inArrayToSeq ar_in midi_frame ;
        match !state () with
        | Cons ((status, d1, d2), tl) ->
            ar_out.{midi_frame} <- status ;
            ar_out.{midi_frame} <- d1 ;
            ar_out.{midi_frame} <- d2
        | Nil ->
            ar_out.{midi_frame} <- 0 ;
            ar_out.{midi_frame} <- 0 ;
            ar_out.{midi_frame} <- 0
      done)
    (fun sr -> sample_rate := float_of_int sr)
