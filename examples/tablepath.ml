let max = !Process.sample_rate *. 0.01 |> Cisp.ofFloat
let tone size freq = Cisp.osc freq |> Cisp.take size

let noise =
  let fromPitch p = p |> ( +. ) 100. |> Cisp.mtof in
  let ps =
    Cisp.walk 0.0 (Cisp.rvf (Cisp.ch [|-(7.1);6.9|]) (Cisp.st 7.1))
    |> Seq.take 12 |> List.of_seq
  in
  let n = List.length ps in
  let slice = max / n in
  let freqs = ps |> List.map fromPitch |> List.to_seq in
  freqs
  |> Seq.concat_map (fun f -> f |> Seq.repeat |> Seq.take slice |> Cisp.osc)
  |> Array.of_seq

let steps =
  Array.init max (fun idx ->
      let next = (idx + 1) mod max in
      Infseq.repeat next)

let get_table arr idx =
  if idx > Array.length arr - 1 then None
  else if idx < 0 then None
  else Some arr.(idx)

let play_index_table_first arr first =
  let rec aux arr idx () =
    match get_table arr idx with
    | Some stream -> (
        match stream () with
        | Infseq.InfCons (nextIdx, tail) ->
            Array.set arr idx tail;
            Infseq.InfCons (idx, aux arr nextIdx))
    | None -> Infseq.InfCons (Toolkit.rvi 0 max, aux arr 0)
  in
  aux arr first

let play_index_table arr = play_index_table_first arr 0

let write_line () =
  let open Cisp in
  let idx = Toolkit.rvi 0 (max - 1) in
  let stream =
    tline (ch [|0.001;0.1;0.2;0.3;0.9|])
      (seq [0.0;Toolkit.rvfi 0.0 (max - 1 |> float_of_int)])
    |> Infseq.cycleSq |> Infseq.map int_of_float
    |> Infseq.map (fun x -> Cisp.clip 0 max x)
  in
  steps.(idx) <- stream

let write_normal idx =
  (* let next = Infseq.repeat (Toolkit.wrap 0 max (idx + 1)) in *)
  let next =
    Cisp.weights [ (idx+1, 10000000); (Toolkit.rvi 0 (max - 1), 1) ]
    |> Infseq.cycleSq
  in
  steps.(idx) <- next

let () =
  let open Cisp in
  let chaos =
       timed
         (tline (st 7.0) (seq [0.01;6.0]))
         (st write_line |> Seq.map (fun x -> x ()))
     in
  let peace =
    timed
      (tline (rvf (st 3.0) (st 8.0)) (seq [0.001;1.0]))
      ((countTill max) |> Seq.map write_normal) 
  in
  let eff = effect_lst masterClock [ chaos;peace ] in
  let signal () =
    let start = Toolkit.rvi 0 max in
    play_index_table_first steps start |> Infseq.index noise |> Infseq.to_seq
  in
  let channels =
    rangei 0 1 |> Seq.map (fun _ -> mkLots 5 signal) |> List.of_seq
  in
  Jack.playSeqs 0 Process.sample_rate (effect eff (mkLots 4 signal) :: channels)
