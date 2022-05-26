(*
let mkBoermanFading2 () =
  let open Cisp in
  let memsize = 10.0 in
  let tabIndex = seq [0.0;sec memsize]  in
  let dura = ch [| 0.5; 2.0/.3.0; 3.0/.2.0; 2.0 |] |> Seq.map (fun x -> x *. memsize)  in
  let readpos = tline dura tabIndex in
  let buffer = Array.make (seci memsize) 0.0 in
  let input = Process.inputSeq 0 |> bhpf_static 100.0 0.9 in
  let writerIdx = countTill <| cap buffer in
  let writer = write buffer writerIdx input in
  let myReader =
    indexCub buffer readpos 
  in
  let joined = effect writer myReader in
  joined 
*)

let mini_peak p width alternative input =
  let f x alt = if x > p && x < p +. width then alt else x in
  Cisp.map2 f input alternative

let mini_peak_dyn psq width alternative input =
  let f p x alt = if x > p && x < p +. width then alt else x in
  Cisp.map3 f psq input alternative

let monitor str sq =
  sq
  |> Seq.map (fun x ->
         Cisp.debugi ("\n" ^ str ^ " ") x;
         x)

let monitor_f str sq =
  sq
  |> Seq.map (fun x ->
         Cisp.debugf ("\n" ^ str ^ " ") x;
         x)

open Cisp

let bLow = ref 10.0
let bHigh = ref 1000.0

let consume (sq : (unit -> unit) Seq.t) =
  let open Seq in
  match sq () with
  | Cons (effect, rest) ->
      effect ();
      rest
  | Nil -> fun () -> Seq.Nil

let blow_sq =
  triangle (st 0.33)
  |> Seq.map (fun x -> linlin (-1.0) 1.0 9000.0 80000.0 x)
  |> wrRef bLow 

let bhigh_sq =
  triangle (st 0.44)
  |> Seq.map (fun x -> linlin (-1.0) 1.0 9000.0 80000.0 x)
  |> wrRef bLow

let loopr () =
  let open Cisp in
  let input = Process.inputSeq 0 in
  (* let input =  in  *)
  let memsize = seci 10.0 in
  let buffer = Array.make memsize 0.0 in
  let writerIdx = countTill <| cap buffer in
  let writer = write buffer writerIdx input in
  let starts = timed (lift rvf 1.0 3.0) (lift rvf 0.0 10.0) |> Seq.map sec in
  let bLow = rdRef bLow in
  let bHigh = rdRef bHigh in
  let durations =
    timed
      (fractRandTimer (ch [| 1.7; 3.2; 2.0; 3.0; 4.0 |]))
      (rvf bLow bHigh |> Seq.map int_of_float
      |> Seq.map (fun x -> if x < 0 then 99999 else x))
  in
  let ratios = timed (st 4.0) (ch [| 0.5; 1.0; 2.0 |]) in
  let readIdx = ramps starts durations ratios in
  let reader = indexLin buffer readIdx in
  effect writer reader

let clock = generator (fun () -> Some (getSampleCount ()))

let eff =
  clock
  |> Seq.map (fun i ->
         if i mod 48000 = 0 then (
           print_string ".";
           flush stdout)
         else ())

let all_channels =
  Cisp.rangei 0 14 |> List.of_seq |> List.map (fun _ -> loopr ())

let () =
  let open Cisp in
  let f () =
    let () =
      (* let clock = Seq.map (fun x () -> x) masterClock in *)
      (* let effects = effect_lst [blow_sq;bhigh_sq;clock] in *)
      let effs = effect_lst masterClock [ blow_sq; bhigh_sq ] in
      let first = effect effs (loopr ()) in
      Jack.playSeqs 1 Process.sample_rate (first :: all_channels)
    in
    while true do
      Unix.sleep 60
    done
  in
  let _ = Thread.create f () in
  let _ =
    List.map
      (fun str -> Sys.command str |> ignore)
      [ "jack_connect system:capture_1 ocaml:input_0"; "jack_lsp -c" ]
  in
  while true do
    Unix.sleep 60
  done
