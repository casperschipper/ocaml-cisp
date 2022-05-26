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

(* any array, use a signal (-1.0 < signal < 1.0) to lookup values in array *)
let lookup_signal_array (arr:'a Array.t) (signal:float Seq.t) =
  let len = 
    Array.length arr 
  in
  let make_index f = 
    f 
    |> (linlin (-1.0) 1.0 0.0 (len |> float_of_int))
    |> floor 
    |> int_of_float 
    |> indexArr len arr
  in
  signal |> Seq.map make_index 

let blow_sq =
  triangle (st (1.0 /. 13.0))
  |> lookup_signal_array [|10.0;20.0;30.0;512.0;1024.0;16384.0;65536.0|]
  |> wrRef bLow 

let bhigh_sq =
  triangle (st (1.0 /. 11.0))
  |> lookup_signal_array [|10.0;20.0;30.0;512.0;1024.0;16384.0;65536.0|]
  |> wrRef bLow

let loopr () =
  let open Cisp in
  let input = Process.inputSeq 0 in
  (* let input =  in  *)
  let memsize = seci 10.0 in
  let buffer = Array.make memsize 0.0 in
  let writerIdx = countTill <| cap buffer in
  let writer = write buffer writerIdx input in
  let starts = tline_start (Toolkit.rvfi 1.0 5.0) (lift rvf 1.0 5.0) (seq [0.0;10.0]) |> Seq.map sec in
  let bLow = rdRef bLow in
  let bHigh = rdRef bHigh in
  let durations =
    timed
      (lift rvf 0.1 2.0)
      (rvf bLow bHigh |> Seq.map int_of_float
      |> Seq.map (fun x -> if x < 0 then 99999 else x))
  in
  let ratios = timed (triangle (st 0.01) |> Seq.map (linlin (-1.0) 1.0 0.01 1.0)) (seq [-4.0;-1.0;0.5;1.0;2.0;0.25]) in
  let readIdx = ramps starts durations ratios in
  let reader = indexCub buffer readIdx in
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
