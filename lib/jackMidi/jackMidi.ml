external open_midi_stream :
     (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
  -> (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
  -> (int -> unit)
  -> (int -> unit)
  -> unit = "open_midi_stream"

let playMidi midiInToOutFunction sample_rate =
  let open Seq in
  let ar_out =
    Bigarray.Array1.create Bigarray.Int8_unsigned Bigarray.c_layout 16384
  in
  let ar_in =
    Bigarray.Array1.create Bigarray.Int8_unsigned Bigarray.c_layout 16384
  in
  open_midi_stream ar_out ar_in
    (fun nframes ->
      for i = 0 to nframes - 1 do
        let midi_frame = i * 3 in
        let status, d1, d2 =
          midiInToOutFunction
            (ar_in.{midi_frame}, ar_in.{midi_frame + 1}, ar_in.{midi_frame + 2})
        in
        ar_out.{midi_frame} <- status ;
        ar_out.{midi_frame + 1} <- d1 ;
        ar_out.{midi_frame + 2} <- d2
      done)
    (fun sr -> sample_rate := float_of_int sr)

