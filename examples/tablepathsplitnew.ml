
let n_channels = 16


let () = Random.init 3


let readFile () = 
  let snd = Sndfile.read "/Users/casperschipper/Music/Null/12_channel_thunder.wav" in
  (* let snd_n_channels = Sndfile.n_channels snd in *)
  let array = Sndfile.to_seq snd 0 |> Cisp.drop (44100 * 0) |> Cisp.take (44100 * 1) |> Array.of_seq in 
  let n_samps = Array.length array in
  let _ = Cisp.debugf "\n\nlength of file in seconds: " (n_samps |> fun x -> x |> float_of_int |>  fun x -> x /. 44100.0 ) in
  let _ = Cisp.debugi "\n\nsize in samps" n_samps in
  (array,n_samps)
  (* Cisp.rangei 0 n_channels |> Seq.map (Sndfile.to_seq snd) |> List.of_seq *)

let max = 220 


let sawtooth size =
  let open Cisp in 
  let pow x = Float.pow x 10.0 in
  let stepsize = 2.0 /. (float_of_int size) in 
  walk (-1.0) (st stepsize) |> fmap pow |> take size |> Array.of_seq 

let buff = sawtooth max


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

let fractHold lst =
  let open Cisp in
  let lstarr = lst |> Array.of_list in
  hold (ch lstarr |> hold (ch lstarr))

let write_split () =
  let open Toolkit in 
  let idx = rvi 1 (max - 1) in
  let stream = 
    let open Cisp in
    let a = idx + 1  in
    let b = rvi 0 (max-1) in 
    ch [|a;b|] |> hold (lift rv 1 10) |> Infseq.cycleSq
  in
  Array.iter (fun array -> array.(idx) <- stream) steps

let write_normal idx =
  let next = Infseq.repeat ((idx + 1) mod (max-1)) in 
  Array.iter (fun array -> array.(idx) <- next) steps



let () = 
  let open Cisp in

  let chaos = timed (fractRandTimer (ch [|0.1;0.2;0.3;0.5;1.1;2.0|])) (st write_split |> Seq.map (fun x -> x ()) ) in 
  let peace = timed (fractRandTimer (ch [|0.1;0.2;0.3;0.5;1.1;2.0|])) (countTill max |> Seq.map write_normal) in
  let eff = effect_lst masterClock [chaos;peace] in 
  let signal channel = play_index_table steps.(channel) |> Infseq.index (sawtooth 4096) |> Infseq.to_seq |> Seq.map (fun x -> x *. 0.1) |> Cisp.bhpf_static 150.0 0.99 in
  let channels = rangei 1 (n_channels-1)  |> Seq.map (fun n -> signal n) |> List.of_seq in
  
  let with_effect = ((effect eff (signal 0)) :: channels) in
   if true then
    let size = !Process.sample_rate *. 90.0 |> int_of_float in
    let t = Sndfile.from_seq size (int_of_float !Process.sample_rate) with_effect in
    Sndfile.write t "/Users/casperschipper/Music/Null/pulse_split_recycle_range.wav" Sndfile.WAV_32
  else 
    Jack.playSeqs 0 Process.sample_rate with_effect
  