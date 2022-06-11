let max = 16
let noise = Cisp.sineseg max |> Array.of_seq

let () = 
  Random.init 1

let write_split_idx () =
  let open Cisp in
  let a = Toolkit.rvi 1 (max - 1) in
  let b = Toolkit.rvi 1 (max - 1) in
  let stream = ch [| a; b |] |> hold ([1;9;11;17;13] |> shuffle |> seq ) |> Infseq.cycleSq in
  stream

let write_normal idx = Infseq.repeat (idx + 1 mod max)

let steps =
  let write_at idx =
    let p = Random.int 100 in
    if p < 7 then write_split_idx () else write_normal idx
  in
  Array.init max (fun idx -> write_at idx)

let get_table arr idx =

  if idx >= Array.length arr || idx < 0 then
    (
    Cisp.debugi "what is going on ?" idx ; 
    None) 
  else Some arr.(idx)

let write_split () =
  let open Cisp in
  let idx = Toolkit.rvi 1 (max - 1) in
  let a = Toolkit.rvi 1 (max - 1) in
  let b = Toolkit.rvi 1 (max - 1) in
  let stream = ch [| a; b |] |> hold (lift rv 100 9000) |> Infseq.cycleSq in

  steps.(idx) <- stream

let write_normal idx =
  let next = Infseq.repeat (idx + 1 mod (max - 1)) in
  steps.(idx) <- next

let play_index_table arr =
  let rec aux arr idx () =
    match get_table arr idx with
    | Some stream -> (
        match stream () with
        | Infseq.InfCons (nextIdx, tail) ->
            Array.set arr idx tail;
            Infseq.InfCons (idx, aux arr nextIdx))
    | None -> Infseq.InfCons (0, aux arr 0)
  in
  aux arr 0

let () =
  let open Cisp in
  let eff = masterClock in
  let signal () =
    play_index_table steps |> Infseq.index noise |> Infseq.to_seq
  in
  Jack.playSeqs 0 Process.sample_rate [ effect eff (signal ()) ]
