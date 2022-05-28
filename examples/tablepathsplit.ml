let max = 1024

let n_channels = 32

let noise = 
  let open Cisp in
  lift rvf (-1.0) 1.0 |> Seq.map (fun x -> Float.pow x 10.0) |> take max |> Array.of_seq

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
  let b = Toolkit.rvi 1 (max - 1) in
  let c = Toolkit.rvi 1 (max -1) in
  let stream = 
    let open Cisp in
    seq [a;b;c] |> hold (lift rv 1 20) |> Infseq.cycleSq
    (* Infseq.ch_seq [|(Cisp.ch [|idx;idx;idx;idx;a|] |> Infseq.cycleSq);Infseq.repeat idx|]  *)
  in
  Array.iter (fun array -> array.(idx) <- stream) steps

let write_normal idx =
  let next = Infseq.repeat ((idx + 1) mod (max-1)) in 
  Array.iter (fun array -> array.(idx) <- next) steps



let () = 
  let open Cisp in

  let chaos = timed (fractRandTimer (ch [|0.001;0.1;1.0;2.0|])) (st write_split |> Seq.map (fun x -> x ()) ) in 
  let peace = timed (fractRandTimer (ch [|0.001;0.1;0.1;1.0;2.0|])) (countTill max |> Seq.map write_normal) in
  let eff = effect_lst masterClock [chaos;peace] in 
  let signal channel = play_index_table steps.(channel) |> Infseq.index noise |> Infseq.to_seq in
  let channels = rangei 1 (n_channels-1)  |> Seq.map (fun n -> signal n) |> List.of_seq in
  
  let with_effect = ((effect eff (signal 0)) :: channels) in
   if true then
    let size = !Process.sample_rate *. 180.0 |> int_of_float in
    let t = Sndfile.from_seq size (int_of_float !Process.sample_rate) with_effect in
    Sndfile.write t "/Users/casperschipper/Music/Null/tablepath_pulse_4_split.wav" Sndfile.WAV_32
  else 
    Jack.playSeqs 0 Process.sample_rate with_effect
  