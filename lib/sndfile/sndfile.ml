external caml_snd_write :
     (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t
  -> string
  -> int * int * int * int
  -> unit = "caml_snd_write"

external caml_snd_read :
  string -> (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t
  = "caml_snd_read"

external caml_n_channels : string -> int = "caml_n_channels"

external caml_n_samplerate : string -> int = "caml_n_samplerate"

type t =
  { channels: int
  ; sr: int
  ; buffer: (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t
  }

type format =
  | WAV_8
  | WAV_16
  | WAV_24
  | WAV_32
  | WAV_FLOAT
  | AIFF_8
  | AIFF_16
  | AIFF_24
  | AIFF_32
  | AIFF_FLOAT
  | RAW_8
  | RAW_16
  | RAW_24
  | RAW_32
  | RAW_FLOAT

let make_format_tuple sr chns = function
  | WAV_8 -> (sr, chns, 0x0001, 0x010000)
  | WAV_16 -> (sr, chns, 0x0002, 0x010000)
  | WAV_24 -> (sr, chns, 0x0003, 0x010000)
  | WAV_32 -> (sr, chns, 0x0004, 0x010000)
  | WAV_FLOAT -> (sr, chns, 0x0006, 0x010000)
  | AIFF_8 -> (sr, chns, 0x0001, 0x020000)
  | AIFF_16 -> (sr, chns, 0x0002, 0x020000)
  | AIFF_24 -> (sr, chns, 0x0003, 0x020000)
  | AIFF_32 -> (sr, chns, 0x0004, 0x020000)
  | AIFF_FLOAT -> (sr, chns, 0x0006, 0x020000)
  | RAW_8 -> (sr, chns, 0x0001, 0x040000)
  | RAW_16 -> (sr, chns, 0x0002, 0x040000)
  | RAW_24 -> (sr, chns, 0x0003, 0x040000)
  | RAW_32 -> (sr, chns, 0x0004, 0x040000)
  | RAW_FLOAT -> (sr, chns, 0x0006, 0x040000)

let write_buf array string sr chns format =
  caml_snd_write array string (make_format_tuple sr chns format)

let snd_channels = caml_n_channels

let snd_sr = caml_n_samplerate

let snd_read = caml_snd_read

let write snd string format =
  write_buf snd.buffer string snd.sr snd.channels format

let read fname =
  {channels= snd_channels fname; sr= snd_sr fname; buffer= snd_read fname}

let idx snd i = snd.buffer.{i}

(* t channel number index -> returns samples*)
let idx_channel snd c i = snd.buffer.{(snd.channels * i) + c}

let n_frames snd = Bigarray.Array1.dim snd.buffer / snd.channels

let channels snd = snd.channels

let sr snd = snd.sr

let from_seq n sr slst =
  let n_channels = List.length slst in
  let buf =
    Bigarray.Array1.create Bigarray.Float32 Bigarray.c_layout (n * n_channels)
  in
  let seq = Array.of_list slst in
  let open Seq in
  for i = 0 to n - 1 do
    Array.iteri
      (fun c channel_stream ->
        let buf_i = (i * n_channels) + c in
        match channel_stream () with
        | Nil -> buf.{buf_i} <- 0.
        | Cons (a, next) -> buf.{buf_i} <- a ; seq.(c) <- next)
      seq
  done ;
  {channels= n_channels; sr; buffer= buf}

let fromProc n sr glst =
  let _ = Process.sample_rate := float_of_int sr in
  let n_channels = List.length glst in
  let buf =
    Bigarray.Array1.create Bigarray.Float32 Bigarray.c_layout (n * n_channels)
  in
  let seq = Array.of_list glst in
  for i = 0 to n - 1 do
    Array.iteri
      (fun c gen ->
        let buf_i = (i * n_channels) + c in
        buf.{buf_i} <- Process.generate_next gen)
      seq
  done ;
  {channels= n_channels; sr; buffer= buf}

let toProc snd channel =
  Process.map (idx_channel snd channel) (Process.inc_int 0 1)

  (* TODO this should return a result, if the channel number is incorrect *)
let to_seq snd channel =
  let count =
    let number = n_frames snd in
    let rec aux n () =
      if n == number then
         Seq.Nil
      else
         Seq.Cons(n, aux (n + 1))
    in
    aux 0
  in
  Seq.map (idx_channel snd channel) count

let n_channels snd =
  snd.channels
