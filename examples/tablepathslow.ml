let max = 16384
let noise = 
  let open Cisp in
  lift rvf (-1.0) 1.0 |> Seq.map (fun x -> Float.pow x 30.0) |> take max |> Array.of_seq

let steps =
  Array.init max (fun idx -> let next = ((idx + 1) mod max) in Infseq.repeat next)

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
  let b = Toolkit.rvi 1 3 in
  let p = Cisp.pickOne [|10;44;20;40;50;100;1000;2000;3000;4000;100;200;3000;400|] in 
  let stream = 
    Infseq.sometimes ((idx+b) mod max) a p 
  in
  steps.(idx) <- stream

let write_normal idx =
  let next = Infseq.repeat (idx + 1) in 
  steps.(idx) <- next 

  

let () = 
  let open Cisp in

  let chaos = timed (fractRandTimer (ch [|0.1;1.0|])) (st write_split |> Seq.map (fun x -> x ()) ) in 
  let peace = timed (fractRandTimer (ch [|0.001;0.1;1.0|])) (countTill 255 |> Seq.map write_normal) in
  let eff = effect_lst masterClock [chaos;peace] in 
  let signal () = play_index_table steps |> Infseq.index noise |> Infseq.to_seq in
  let channels = rangei 0 14 |> Seq.map (fun _ -> signal ()) |> List.of_seq in
  Jack.playSeqs 0 Process.sample_rate ((effect eff (signal ())) :: channels)
 