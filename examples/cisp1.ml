(* Generate a finite sequence of length n from a function f : int -> float *)
let seq_of_fun n f =
  let rec aux i () =
    if i >= n then Seq.Nil
    else Seq.Cons (f i, aux (i+1))
  in aux 0

(* Basic waveforms *)
let sine f sr n =
  seq_of_fun n (fun i ->
    let t = float i /. float sr in
    sin (2. *. Float.pi *. f *. t)
  )

let saw f sr n =
  seq_of_fun n (fun i ->
    let t = float i /. float sr in
    2. *. ((f *. t) -. floor (0.5 +. f *. t))
  )

let square f sr n =
  seq_of_fun n (fun i ->
    let t = float i /. float sr in
    if sin (2. *. Float.pi *. f *. t) >= 0. then 1. else -1.
  )

let triangle f sr n =
  seq_of_fun n (fun i ->
    let t = float i /. float sr in
    2. *. abs_float (2. *. ((f *. t) -. floor (0.5 +. f *. t))) -. 1.
  )

(* FM and PM waves *)
let fm f fm_index fm_rate sr n =
  seq_of_fun n (fun i ->
    let t = float i /. float sr in
    sin ((2. *. Float.pi *. f *. t) +.
         fm_index *. sin (2. *. Float.pi *. fm_rate *. t))
  )

let pm f pm_index pm_rate sr n =
  seq_of_fun n (fun i ->
    let t = float i /. float sr in
    sin (2. *. Float.pi *. f *. t +.
         pm_index *. sin (2. *. Float.pi *. pm_rate *. t))
  )

(* Additive mixtures *)
let additive freqs amps sr n =
  seq_of_fun n (fun i ->
    let t = float i /. float sr in
    List.fold_left2
      (fun acc f a -> acc +. a *. sin (2. *. Float.pi *. f *. t))
      0. freqs amps
  )

(* Filtered noise via simple 1-pole lowpass *)
let filtered_noise cutoff sr n =
  let a = exp (-. 2. *. Float.pi *. cutoff /. float sr) in
  let rec aux i prev () =
    if i >= n then Seq.Nil
    else
      (* random noise *)
      let x = (Random.float 2. -. 1.) in
      let y = a *. prev +. (1. -. a) *. x in
      Seq.Cons (y, aux (i+1) y)
  in aux 0 0.0

(* Chaotic logistic oscillator *)
let logistic r x0 sr n =
  let rec aux i x () =
    if i >= n then Seq.Nil
    else
      let x' = r *. x *. (1. -. x) in
      Seq.Cons (2. *. x' -. 1., aux (i+1) x')
  in aux 0 x0

(* Main generator: produce 128 diverse waveshapes *)
let generate_waveforms ~sample_rate ~length =
  let base_freqs = List.init 16 (fun _ -> 10. +. float (Toolkit.rvi 0 1 * 12)) in
  let pick lst idx = List.nth lst (idx mod List.length lst) in

  Array.init 32 (fun i ->
    match i mod 8 with
    | 0 -> sine (pick base_freqs i) sample_rate length
    | 1 -> saw (pick base_freqs i) sample_rate length
    | 2 -> square (pick base_freqs i) sample_rate length
    | 3 -> triangle (pick base_freqs i) sample_rate length

    | 4 ->
        fm (pick base_freqs i) (0.5 +. float (i mod 5)) 3. sample_rate length

    | 5 ->
        pm (pick base_freqs i) (0.8 +. float (i mod 7)) 2.5 sample_rate length

    | 6 ->
        let f = pick base_freqs i in
        let freqs = [f; 2.*.f; 3.*.f] in
        let amps  = [0.7; 0.3; 0.15] in
        additive freqs amps sample_rate length

    | 7 ->
        (* chaotic or filtered noise depending on index *)
        if i mod 16 < 8 then
          filtered_noise (100. +. float (i * 10)) sample_rate length
        else
          logistic (3.6 +. 0.01 *. float i) 0.1 sample_rate length

    | _ -> Seq.repeat 0.0 |> Seq.take 128
  )



let sawdust () =
  let open Cisp in
  let waveformarray = generate_waveforms ~sample_rate:(int_of_float !Process.sample_rate) ~length:32 in
  let indexer = rv (st 0) (st (Array.length waveformarray)) in
  let holder = indexer |> hold (lift rv 100 1000) in
  let seqer = holder |> fmap (fun idx -> Some waveformarray.(idx)) |> Seq.filter_map id in
  seqer |> concat |> fmap (fun x -> Toolkit.clipf (-1.0) (1.0) x) |> hold (st 1)




let () =
  let open Cisp in
  Jack.playSeqs 0 Process.sample_rate [effectSync Cisp.masterClock (sawdust ()); sawdust () ] ;
  Unix.sleep 1 ;
  ignore (Sys.command "jack_connect ocaml:playback_1 ocaml:output_0")
