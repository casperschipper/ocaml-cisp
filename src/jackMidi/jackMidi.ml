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

let playMidi midiFun sample_rate =
  let open Seq in
  let ar_out =
    Bigarray.Array1.create Bigarray.Int8_unsigned Bigarray.c_layout 4096
  in
  let ar_in =
    Bigarray.Array1.create Bigarray.Int8_unsigned Bigarray.c_layout 4096
  in
  open_midi_stream ar_out ar_in
    (fun nframes ->
      for i = 0 to nframes - 1 do
        let midi_frame = i * 3 in
        let midiMsgIn =
          (ar_in.{midi_frame}, ar_in.{midi_frame + 1}, ar_in.{midi_frame + 2})
        in
        let status, data1, data2 = midiFun midiMsgIn in
        ar_out.{midi_frame} <- status ;
        ar_out.{midi_frame + 1} <- data1 ;
        ar_out.{midi_frame + 2} <- data2
      done)
    (fun sr -> sample_rate := float_of_int sr)
