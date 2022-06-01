
let n_channels = 16

(*
let structnoise section_length bias randomness = 
  let open Cisp in
  let maybe p =
    Random.float 1.0 < p
  in
  let seed length =
    let half = length / 2 in
    let rest = length - half in
    (half,rest)
  in
  let rec mkSection p sec_length (zeros,ones) () =
    if sec_length = 0 then
      Seq.Nil
    else
      if maybe p then
        match ones with 
        0 -> mkSection
*)

let readFile () = 
  let snd = Sndfile.read "/Users/casperschipper/Music/Null/12_channel_thunder.wav" in
  (* let snd_n_channels = Sndfile.n_channels snd in *)
  let array = Sndfile.to_seq snd 0 |> Cisp.take (44100 * 3) |> Array.of_seq in 
  let n_samps = Array.length array in
  let _ = Cisp.debugf "\n\nlength of file in seconds: " (n_samps |> fun x -> x |> float_of_int |>  fun x -> x /. 44100.0 ) in
  let _ = Cisp.debugi "\n\nsize in samps" n_samps in
  (array,n_samps)
  (* Cisp.rangei 0 n_channels |> Seq.map (Sndfile.to_seq snd) |> List.of_seq *)

let (noise,max) = readFile ()


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
  hold (ch lstarr |> hold (ch lstarr |> hold (ch lstarr)))

let write_split () =
  let idx = Toolkit.rvi 1 (max - 1) in

  let stream = 
    let open Cisp in
    let a = Toolkit.rvi 0 (max - 1) in
    let _ = debugi "\na = " a in
    seq [idx+1 mod max;0;a] |> fractHold ([2;3;5;10;33;300;300]) |> Infseq.cycleSq
    (* Infseq.ch_seq [|(Cisp.ch [|idx;idx;idx;idx;a|] |> Infseq.cycleSq);Infseq.repeat idx|]  *)
  in
  Array.iter (fun array -> array.(idx) <- stream) steps

let write_normal idx =
  let next = Infseq.repeat ((idx + 1) mod (max-1)) in 
  Array.iter (fun array -> array.(idx) <- next) steps



let () = 
  let open Cisp in

  let chaos = timed (fractRandTimer (ch [|0.001;0.1;0.1;1.0;2.0|])) (st write_split |> Seq.map (fun x -> x ()) ) in 
  let peace = timed (fractRandTimer (ch [|0.1;0.1;1.0;2.0|])) (countTill max |> Seq.map write_normal) in
  let eff = effect_lst masterClock [chaos;peace] in 
  let signal channel = play_index_table steps.(channel) |> Infseq.index noise |> Infseq.to_seq |> Seq.map (fun x -> x *. 10.0) |> Cisp.bhpf_static 200.0 0.9 in
  let channels = rangei 1 (n_channels-1)  |> Seq.map (fun n -> signal n) |> List.of_seq in
  
  let with_effect = ((effect eff (signal 0)) :: channels) in
   if true then
    let size = !Process.sample_rate *. 30.0 |> int_of_float in
    let t = Sndfile.from_seq size (int_of_float !Process.sample_rate) with_effect in
    Sndfile.write t "/Users/casperschipper/Music/Null/rain_table_short.wav" Sndfile.WAV_32
  else 
    Jack.playSeqs 0 Process.sample_rate with_effect
  