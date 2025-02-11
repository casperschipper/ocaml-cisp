open Cisp
open Toolkit

let square_wave_from_freq sr frequency_seq () =
  let rec generate_square_wave freq_seq phase =
    Seq.(
      match freq_seq () with
      | Nil -> Nil
      | Cons (freq, rest) ->
          let phase_increment = freq /. sr in
          let new_phase = mod_float (phase +. phase_increment) 1.0 in
          let square_value = if phase < 0.5 then 1.0 else -1.0 in
          Cons (square_value, fun () -> generate_square_wave rest new_phase) )
  in
  generate_square_wave frequency_seq 0.0

let f1 input =
  let low = rvfi 30.0 40. in
  let high = rvfi 60. 128. in
  let input = Process.inputSeq input in
  let envelope = line_chain (seq [0.0; 1.0]) (rv (st 2) (st 40000)) in
  input
  |> fmap (linlin (-1.0) 1.0 low high)
  |> fmap mtof
  |> square_wave_from_freq !Process.sample_rate
  |> mup envelope

let matrix_size = 6

let print_report i o =
  Printf.printf "Have created a thing with %i %i" i o

let matrix n =
  let ins = rangei 0 n in
  let outs = rangei 0 n in
  let final = ins |> Seq.concat_map (fun in1 -> outs |> Seq.map (fun out -> print_report in1 out; f1 in1 )) |> List.of_seq  in
  let _ = print_int (List.length final) in
  let _ = print_endline "" in
  final

(* summing happens at the inputs, there is only one output 

*)

let _ = Jack.playSeqs 0 Process.sample_rate (matrix matrix_size)
