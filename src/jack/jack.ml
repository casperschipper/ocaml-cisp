open Seq

external open_stream :
     (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t
  -> (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t
  -> (int -> unit)
  -> int * int
  -> (int -> unit)
  -> unit = "open_stream"

(* unit "open_stream" *)

let play in_channels sample_rate proc_lst =
  let streams = Array.of_list proc_lst in
  let out_channels = Array.length streams in
  let ar_out =
    Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout
      (1024 * out_channels)
  in
  let ar_in =
    Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout
      (1024 * in_channels)
  in
  open_stream ar_out ar_in
    (fun nframes ->
      Process.input_channels := in_channels ; (* just the number of channels *)
      for i = 0 to nframes - 1 do
        for k = 0 to in_channels - 1 do
          Process.input_array.(k) <- float_of_int i /. float_of_int nframes ; (* input values are written to 'global' array in Process, so not local to Process.t *)
          Process.input_array.(k) <- ar_in.{(i * in_channels) + k}
        done ;
        Array.iteri
          (fun c gen ->
            let buf_i = (i * out_channels) + c in
            ar_out.{buf_i} <- Process.generate_next gen)
          (* that's were I plug in *)
          streams
      done)
    (out_channels, in_channels)
    (fun sr -> sample_rate := float_of_int sr)

let playSeqs in_channels sample_rate seq_lst =
  let streams = Array.of_list seq_lst in
  let out_channels = Array.length streams in
  let ar_out =
    Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout
      (1024 * out_channels)
  in
  let ar_in =
    Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout
      (1024 * in_channels)
  in
  open_stream ar_out ar_in
    (fun nframes ->
      Process.input_channels := in_channels ; (* just the number of channels *)
      for i = 0 to nframes - 1 do
        for k = 0 to in_channels - 1 do
          Process.input_array.(k) <- float_of_int i /. float_of_int nframes ; (* input values are written to 'global' array in Process, so not local to Process.t *)
          Process.input_array.(k) <- ar_in.{(i * in_channels) + k}
        done;
        Array.iteri
          (fun c mySeq ->
            let buf_i = (i * out_channels) + c in
            let rec zeros () = Cons (0.0, zeros) in
            let (head, tail) =
              match mySeq () with
                  | Cons (h,tl) -> (h, tl)
                  | Nil -> (0.0,zeros) (* zero forever *)
            in
            streams.(c) <- tail;
            ar_out.{buf_i} <- head)
          (* that's were I plug in *)
          streams
      done)
    (out_channels, in_channels)
    (fun sr -> sample_rate := float_of_int sr)

(*
let play_cisp sample_rate cisp_lst =
  let streams = Array.of_list proc_lst in
  let out_channels = Array.length streams in
  let ar_out =
    Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout
      (1024 * out_channels)
  in
  open_stream ar_out []
    (fun nframes ->
      Process.input_channels := in_channels ;
      for i = 0 to nframes - 1 do
        Array.iteri
          (fun c gen ->
            let buf_i = (i * out_channels) + c in
            ar_out.{buf_i} <- Process.generate_next gen)
          (* that's were I plug in *)
          streams
      done)
    (out_channels, in_channels)
    (fun sr -> sample_rate := float_of_int sr) *)

(* TODO
 * midi in
 *)

(* let play_lst sample_rate stream =
 *   let first = Process.this stream in
 *   let stream_ref = ref stream in
 *   let n_channels = match first with Some l -> List.length l | _ -> 0 in
 *   let ar =
 *     Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout (1024 * n_channels)
 *   in
 *   open_stream ar
 *     (fun nframes ->
 *       for i = 0 to nframes - 1 do
 *         match !stream_ref () with
 *         | Nil ->
 *             for c = 0 to n_channels - 1 do
 *               ar.{(i * n_channels) + c} <- 0.0
 *             done
 *         | Cons (lst, nextStream) ->
 *             let () = stream_ref := nextStream in
 *             List.iteri (fun c f -> ar.{(i * n_channels) + c} <- f) lst
 *       done)
 *     n_channels
 *     (fun sr -> sample_rate := sr) *)

(* let play_generator sample_rate gen_lst =
 *   let streams = Array.of_list gen_lst in
 *   let n_channels = Array.length streams in
 *   let ar =
 *     Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout
 *       (1024 * Array.length streams)
 *   in
 *   let sc = ref 0 in
 *   open_stream ar
 *     (fun nframes ->
 *       for i = 0 to nframes - 1 do
 *         Array.iteri
 *           (fun c channel_stream ->
 *             ar.{(i * n_channels) + c} <- Generator.generate channel_stream !sc)
 *           streams ;
 *         sc := !sc + 1
 *       done)
 *     n_channels
 *     (fun sr -> sample_rate := sr) *)
