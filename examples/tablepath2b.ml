let max = 512 + 64

let () = Random.init 3

let noise =
  let open Cisp in
  sineseg max |> Array.of_seq

let steps =
  Array.init max (fun idx ->
      let next = (idx + 1) mod max in
      Infseq.repeat next)

let get_table arr idx =
  if idx > Array.length arr || idx < 0 then None else Some arr.(idx)

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

let write_split () =
  let idx = Toolkit.rvi 1 (max - 1) in
  let a = Toolkit.rvi 1 idx in
  let b = Toolkit.rvi 1 idx in
  let o = Toolkit.rvi 10 5001 in
  let stream =
    if Toolkit.rvi 0 30 > 28 then
      Cisp.ch [| a; b |] |> Cisp.hold (Cisp.st o) |> Infseq.cycleSq
    else Cisp.seq [ a; b ] |> Cisp.hold (Cisp.st o) |> Infseq.cycleSq
  in
  steps.(idx) <- stream

let write_normal idx =
  let next = Infseq.repeat (idx + 1) in
  steps.(idx) <- next

let () =
  let open Cisp in
  let chaos =
    timed
      (fractRandTimer (2.0 *.- ch [| 0.001; 1.5; 0.75 |]))
      (st write_split |> Seq.map (fun x -> x ()))
  in
  let peace =
    timed
      (fractRandTimer (2.0 *.- ch [| 0.001; 0.1;|]))
      (countTill (max - 1) |> Seq.map write_normal)
  in
  let eff = effect_lst masterClock [ chaos; peace ] in
  let signal () =
    play_index_table steps |> Infseq.index noise |> Infseq.to_seq
  
  in
  let channels = rangei 0 14 |> Seq.map (fun _ -> signal ()) |> List.of_seq in
  let with_effect = effect eff (signal ()) :: channels in
  if true then
    let size = !Process.sample_rate *. (60.0 *. 3.0) |> int_of_float in
    let t =
      Sndfile.from_seq size (int_of_float !Process.sample_rate) with_effect
    in
    Sndfile.write t "/Users/casperschipper/Music/Null/BIRDNOTE.wav"
      Sndfile.WAV_32
  else Jack.playSeqs 0 Process.sample_rate with_effect
