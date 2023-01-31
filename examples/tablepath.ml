let max = !Process.sample_rate *. 3.5 |> Cisp.ofFloat

let tone size freq = Cisp.osc freq |> Cisp.take size

let noise =
  let fromPitch p =
    p |> ( +. ) 24. |> Cisp.mtof
  in
  let ps = 
    Cisp.walk 0.0 (Cisp.rvf (Cisp.st 6.9) (Cisp.st 7.1)) |> Seq.take 12 |> List.of_seq
  in
  let n = List.length ps in
  let slice = max / n in
  let freqs =
    ps |> List.map fromPitch |> List.to_seq
  in
  freqs |> Seq.concat_map (fun f -> f |> Seq.repeat |> Seq.take slice |> Cisp.osc) |> Array.of_seq

let steps =
  Array.init max (fun idx ->
      let next = (idx + 1) mod max in
      Infseq.repeat next)

let get_table arr idx =
  if idx > (Array.length arr -1) || idx < 0 then None else Some arr.(idx)

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
  let a = Toolkit.rvi 1 (max - 1) in
  let b = Toolkit.rvi 1 (max - 1) in
  let o = Toolkit.rvi 10 11 in
  let stream =
    if Toolkit.rvi 0 30 > 28 then
      Cisp.ch [| a; b |] |> Cisp.hold (Cisp.st o) |> Infseq.cycleSq
    else Cisp.seq [ a; b ] |> Cisp.hold (Cisp.st o) |> Infseq.cycleSq
  in
  steps.(idx) <- stream

  let write_line () =
    let idx = Toolkit.rvi 1 (max - 1) in
    let stream =
      Cisp.line (Cisp.seq [0.0;max |> float_of_int]) (Cisp.st 41.0) |> Infseq.cycleSq |> Infseq.map int_of_float
    in
    steps.(idx) <- stream

let write_normal idx =
  let next = Infseq.repeat (idx + 1) in
  steps.(idx) <- next

let () =
  let open Cisp in
  let chaos =
    timed
      (fractRandTimer (ch [| 0.1; 0.5; 1.0; 2.0; 3.0; 4.0 |]))
      (st write_line |> Seq.map (fun x -> x ()))
  in
  let peace =
    timed
      (fractRandTimer (ch [|0.1; 0.5|]))
      (countTill 255 |> Seq.map write_normal)
  in
  let eff = effect_lst masterClock [ chaos; peace ] in
  let signal () =
    play_index_table steps |> Infseq.index noise |> Infseq.to_seq
  in
  let channels = rangei 0 14 |> Seq.map (fun _ -> signal ()) |> List.of_seq in
  Jack.playSeqs 0 Process.sample_rate (effect eff (signal ()) :: channels)
