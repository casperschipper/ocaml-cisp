let max = 1024
let noise = Array.init max (fun _ -> Toolkit.rvfi (-1.0) (1.0))

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
  let b = Toolkit.rvi 1 (max - 1) in
  let o = Toolkit.rvi 1 100 in
  let stream = Cisp.ch [|a;b|] |> Cisp.hold (Cisp.st o) |> Infseq.cycleSq in
  steps.(idx) <- stream


let () = 
  let open Cisp in
  let eff = effect_lst masterClock [timed (st 1.0) (st write_split |> Seq.map (fun x -> x ()) )] in 
  let signal () = play_index_table steps |> Infseq.index noise |> Infseq.to_seq in
  Jack.playSeqs 0 Process.sample_rate ([effect eff (signal ())])
 