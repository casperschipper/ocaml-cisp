open Cisp

let f = resetWalk mupWalk 440.0
let ar = Array.of_list

let makeIntervals =
  let intervals =
    rangei 1 4 |> floatify |> Seq.map (fun x -> x /. (x +. 1.0))
  in
  let inverse = intervals |> Seq.map (fun x -> 1.0 /. x) in
  append intervals inverse |> Array.of_seq

let table =
  [ st 1; st 2; st 3; st 4; st 5; [ 0; 0; 0; 3 ] |> ar |> ch ] |> Array.of_list

let graph_synth =
  let interv = makeIntervals in
  let update () state =
    let next = table.(state) in
    match next () with
    | Nil -> 0
    | Cons (x, xs) ->
        table.(state) <- xs;
        x
  in
  let indexer = recursive (st ()) 0 update (fun x -> x) in
  indexer
  |> Seq.map (fun i -> interv.(i))
  |> f
  |> hold (st 100)
  |> Seq.map (fun x -> 4410 / int_of_float (x +. 1.0) |> sineseg)
  |> concat

let () =
  Jack.playSeqs 0 Process.sample_rate
    [ Cisp.effect Cisp.masterClock graph_synth; graph_synth ];
  Unix.sleep 1;
  ignore (Sys.command "jack_connect ocaml:playback_1 ocaml:output_0");
  ignore (Unix.sleep 1000)
