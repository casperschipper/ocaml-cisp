open Cisp

(*
let () = 
  let env = triangle (st 50.0) |> Seq.map (linlin (-1.0) 1.0 400.0 12000.0) in
  let oscil () = osc env in
  Jack.playSeqs 0 Process.sample_rate [effect masterClock (oscil ());oscil ();oscil ();oscil ()]
*)

let topD, bottomD = ref 0.0, ref 0.0
let topAmp,bottomAmp = ref 1.0, ref 1.0 
let topArray = ref 10.0

let mups = 4.0

let time_table = [|0.001;7.0;1.0;0.2;3.0;5.0;10.0|] 

let a = tline_start 10.0 (mups *.- ch time_table) (ch [|-1.0;1.0;0.0;0.1;-1.0|] |> hold (lift rv 1 2)) |> wrRef topD
let b = tline_start 10.0 (mups *.- ch time_table) (ch [|-1.0;1.0;0.0;0.1;-1.0|] |> hold (lift rv 1 2)) |> wrRef bottomD
let c = tline_start 10.0 (mups *.- ch time_table) (ch [|-1.0;1.0;0.0;0.1;-1.0|] |> hold (lift rv 1 2)) |> wrRef topAmp
let d = tline_start 10.0 (mups *.- ch time_table) (ch [|-1.0;1.0;0.0;0.1;-1.0|] |> hold (lift rv 1 2)) |> wrRef bottomAmp
let e = tline_start 10.0 (mups *.- ch time_table) (ch [|-1.0;1.0;0.0;0.1;-1.0|] |> hold (lift rv 1 2)) |> wrRef topArray


let amp_array = lift rvf (-0.01) 0.01 |> take 128 |> Array.of_seq 


let () = 
  let duras = rvf (rdRef bottomD) (rdRef topD) |> lookup_signal_array [|1;2;2;2;3;4;4;5;2;3;4;5;6;11;23;33;67;145;256;512;900;3012|] in
  let holder = rvf (rdRef bottomAmp) (rdRef topAmp) |> lookup_signal_array [|1;2;3;4;5;10;33;1000;20000|] in 
  (* let mupper = hold (st 44100) (seq [0.5;1.0;0.75;1.5])  in  *)
  (* let modder m input = Cisp.map2 (fun x ms -> x mod ms) input m in *)
  let wrap c x = 
    if x > c then 
      (0.0 +. (x -. c)) 
    else if x < 0.0 then 
      c -. x
    else
      x
  in
  let signal () = index amp_array (Cisp.bounded_walk_control 0.0 wrap (rdRef topArray) (ch [|-1.0;1.0|] |> hold holder) |> intify ) |> hold duras  in 
  let effs = effect_lst masterClock [a;b;c;d;e] in 
  let eff = effect masterClock effs in
  let channels = rangei 0 3 |> Seq.map (fun _ -> signal ()) |> List.of_seq in 
  Jack.playSeqs 0 Process.sample_rate ((effect eff (signal ())) :: channels)
  