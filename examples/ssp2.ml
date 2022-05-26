open Cisp

(*
let () = 
  let env = triangle (st 50.0) |> Seq.map (linlin (-1.0) 1.0 400.0 12000.0) in
  let oscil () = osc env in
  Jack.playSeqs 0 Process.sample_rate [effect masterClock (oscil ());oscil ();oscil ();oscil ()]
*)

let topD, bottomD = ref 0.0, ref 0.0
let topAmp,bottomAmp = ref 1.0, ref 1.0 

let mups = 1.

let a = triangle (st (0.011 *. mups)) |> wrRef topD
let b = triangle (st  (0.007 *. mups)) |> wrRef bottomD
let c = triangle (st  (0.005 *. mups)) |> wrRef topAmp
let d = triangle (st  (0.003 *. mups)) |> wrRef bottomAmp

let amp_array = lift rvf (-1.0) 1.0 |> take 128 |> Array.of_seq 


let () = 
  let duras = rvf (rdRef bottomD) (rdRef topD) |> lookup_signal_array [|1;2;7;6;12;13;14;15;25;66;107;256;1024|] in
   let holder = rvf (rdRef topAmp) (rdRef bottomAmp) |> lookup_signal_array [|1;2;3;4;5;10;12|] in 
  (* let mupper = hold (st 44100) (seq [0.5;1.0;0.75;1.5])  in  *)
  let signal () = index amp_array (walki 0 holder)  |> hold duras in 
  let effs = effect_lst masterClock [a;b;c;d] in 
  let eff = effect masterClock effs in
  let channels = rangei 0 14 |> Seq.map (fun _ -> signal ()) |> List.of_seq in 
  Jack.playSeqs 0 Process.sample_rate ((effect eff (signal ())) :: channels)
  