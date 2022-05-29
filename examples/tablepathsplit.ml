let max = 2048

let n_channels = 16

let () =
  Random.init 5

let noise = 
  let open Cisp in 
  rangei 0 max |> Seq.map (fun x -> x |> float_of_int |> linlin 0.0 (max |> float_of_int) (-1.0) 1.0) |> Array.of_seq

let steps =
  let one_array () = 
    Array.init max (fun idx -> let next = ((idx + 1) mod max) in Infseq.repeat next)
  in
  Array.make n_channels (one_array ())
 
let get_channel_array arr i =
  arr.(i) 

let get_table arr idx =
  if idx > Array.length arr || (idx < 0) then 
    None
  else  
    Some (arr.(idx))

let play_index_table arr =
  let rec aux arr idx () = 
    match get_table arr idx with
    | Some stream -> 
      (match stream () with
      | Infseq.InfCons(nextIdx,tail) -> Array.set arr idx tail ;Infseq.InfCons(idx,aux arr nextIdx))
    | None ->
      Infseq.InfCons(0,aux arr 0)
  in
  aux arr 0 

let write_split () =
  let idx = Toolkit.rvi 1 (max - 1) in
  let a = Toolkit.rvi 1 (max - 1) in
  let b = Toolkit.rvi 1 (idx+1 mod max) in
  let stream = 
    let open Cisp in
    (* [|lift rv 0 (max -1) |> hold ((seq [17;3;5;6;22;33;1;3000;50000] |> hold (ch [|17;3;5;6;22;33;1;3000;50000|])));ch [|a;b|]|> hold (rv (st 1) (seq [11;2;3;4;400;9000;33000;1;2] |> hold (sometimes 1 2 2)))|] |> fun sqarray -> index_seq sqarray (sometimes 0 1 9000) |> Infseq.cycleSq *)
  [|lift rv 0 (max -1) |> hold ((seq [17;3;5;6;22;33;1;3000;50000] |> hold (ch [|17;3;5;6;22;33;1;3000;50000|])));ch [|a;b|]|> hold (rv (st 1) (seq [11;2;3;4;400;9000;33000;1;2] |> hold (sometimes 1 2 2)))|] |> fun sqarray -> index_seq sqarray (sometimes 1 0 100 |> hold (ch [|2;3;40;4000;5000|])) |> Infseq.cycleSq

    (* Infseq.ch_seq [|(Cisp.ch [|idx;idx;idx;idx;a|] |> Infseq.cycleSq);Infseq.repeat idx|]  *)
  in
  Array.iter (fun array -> array.(idx) <- stream) steps

let write_normal idx =
  let next = Infseq.repeat ((idx + 1) mod (max-1)) in 
  Array.iter (fun array -> array.(idx) <- next) steps


let env () =
  let open Cisp in
  tline_start 0.0 (fractRandTimer (ch [|1.5;2.7;3.3;5.5;10.3;20.0|])) (seq [0.0;1.0;1.0;0.0])

let () = 
  let open Cisp in

  let chaos = timed (fractRandTimer (ch [|0.05;0.5;0.14;3.0;1.0;9.0|])) (st write_split |> Seq.map (fun x -> x ()) ) in 
  let peace = timed (fractRandTimer (ch [|2.0;3.0;1.0;1.0|])) (countTill (max-1) |> Seq.map write_normal) in
  let eff = effect_lst masterClock [chaos;peace] in 
  let signal channel = play_index_table steps.(channel) |> Infseq.index noise |> Infseq.to_seq |> (fun sign -> sign *.~ env ()) in
  let channels = rangei 1 (n_channels-1)  |> Seq.map (fun n -> signal n) |> List.of_seq in
  
  let with_effect = ((effect eff (signal 0)) :: channels) in
   if true then
    let size = !Process.sample_rate *. 120.0 |> int_of_float in
    let t = Sndfile.from_seq size (int_of_float !Process.sample_rate) with_effect in
    Sndfile.write t "/Users/casperschipper/Music/Null/sawtooth_with_trouble.wav" Sndfile.WAV_32
  else 
    Jack.playSeqs 0 Process.sample_rate with_effect
  