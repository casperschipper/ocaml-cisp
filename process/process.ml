open Seq

(* Global settings *)

let sample_rate = ref 44100.

let input_array = Array.create_float 64

let input_channels = ref 0

(* Numerical globals *)

let pi = 4.0 *. atan 1.0

let two_pi = 2.0 *. pi

(* Utility functions *)
let pair a b = (a, b)

let calc_ph_inc sr = 2.0 *. pi /. sr

(* type frequency = {frequency: float; samplerate: float}
 * 
 * let freq sr fr = {frequency= fr; samplerate= sr} *)

let rec mkLstN f i = if i <= 0 then [] else f i :: mkLstN f (i - 1)

(* Accessing data and creating sequences *)

let fromLst = List.to_seq

let rec toLst n s =
  if n <= 0 then []
  else match s () with Nil -> [] | Cons (a, seq) -> a :: toLst (n - 1) seq

let this s = match s () with Nil -> None | Cons (a, _) -> Some a

let next s = match s () with Nil -> None | Cons (_, a) -> Some a

let rec const a () = Cons (a, const a)

let ( ~. ) = const

let id_fun (x : float) = x

let id = map id_fun

let rec from_ref r () = Cons (!r, from_ref r)

let print_float_stream label =
  map (fun v ->
      let () = Printf.printf "%s %f\n" label v in
      v)

(* Applying functions *)

let rec zip f a b () =
  match (a (), b ()) with
  | Nil, _ | _, Nil -> Nil
  | Cons (a1, aSeq), Cons (b1, bSeq) -> Cons (f a1 b1, zip f aSeq bSeq)

let map = Seq.map

(* Alrithmetic operations *)

let ( +~ ) = zip ( +. )

let ( *~ ) = zip ( *. )

let ( -~ ) = zip ( -. )

let ( /~ ) = zip ( /. )

let mul amp = map (fun f -> f *. amp)

let sum = List.fold_left ( +~ ) (const 0.0)

(* Audio input *)

let rec input c () = Cons (input_array.(c), input c)

(* Multichannel *)

(* check whether this may break state because we may be evaluating unnecessarily *)
let evert lst_seq =
  match lst_seq () with
  | Nil -> [(fun () -> Nil)]
  | Cons (lst, _) ->
      let n = List.length lst in
      mkLstN (fun i -> map (fun l -> List.nth l (n - i)) lst_seq) n

let split str = [map fst str; map snd str]

let ( |>> ) lst1 lst2 = List.map2 (fun s1 s2 -> s1 |> s2) lst1 lst2

let ( ||> ) lst f = List.map (fun s -> f s) lst

let pan_to_ph p = (p +. 1.) /. 2.0 *. (pi /. 2.0)

let pan2 signal position =
  let ph = map pan_to_ph position in
  let left = map cos ph in
  let right = map sin ph in
  [left *~ signal; right *~ signal]

let pan2_const signal position =
  let ph = pan_to_ph position in
  let left = cos ph in
  let right = sin ph in
  [~.left *~ signal; ~.right *~ signal]

(* let pan2 signal position =
 *   let ph = map (fun p -> (p /. 2.) +. 0.5) position in
 *   let right = map sqrt ph in
 *   let left = map sqrt (map (fun p -> 1. -. p) ph) in
 *   [left *~ signal; right *~ signal] *)

let rec sumPanPh start inc n =
  if n <= 0 then [start] else start :: sumPanPh (start +. inc) inc (n - 1)

let splay str_lst =
  let n = List.length str_lst in
  let positions =
    if n == 0 then [0.] else sumPanPh (-1.) (2. /. float_of_int (n - 1)) (n - 1)
  in
  List.fold_left
    (fun sum this_pair ->
      match (sum, this_pair) with
      | [sumL; sumR], [thisL; thisR] -> [sumL +~ thisL; sumR +~ thisR]
      | _ -> sum)
    [~.0.; ~.0.]
    (List.map2 pan2_const str_lst positions)

(* Recursive processes *)

let simple_recursion_state_cont start_value stream () =
  let current = ref start_value in
  let rec_seq =
    map
      (fun this_value ->
        let () = current := this_value in
        this_value)
      (stream (from_ref current))
  in
  rec_seq

let recursion_state_cont start_value output input () =
  let current = ref start_value in
  let rec_seq =
    map
      (fun this_value ->
        let () = current := this_value in
        this_value)
      (output (input (from_ref current)))
  in
  rec_seq

let recursive_connection start_value output input () =
  Cons (start_value, recursion_state_cont start_value output input ())

let recursive start_value stream () =
  Cons (start_value, simple_recursion_state_cont start_value stream ())

let rec recursion_map start_value output input () =
  let next_value = output (input start_value) in
  Cons (start_value, recursion_map next_value output input)

let integrate increment = recursive 0.0 (fun last -> increment +~ last)

let rec inc start_value increment () =
  Cons (start_value, inc (start_value +. increment) increment)

let rec inc_int start_value increment () =
  Cons (start_value, inc_int (start_value + increment) increment)

(* Delays *)

(* let del1 init stream () = Cons (init, stream) *)
let del1 init stream =
  let mem = ref init in
  map
    (fun v ->
      let output = !mem in
      mem := v ;
      output)
    stream

(* Filters *)

let calc_p freq = 1. -. (2. *. tan (freq /. !sample_rate))

let lpf1 in_proc freq =
  recursive 0.0 (fun last ->
      let p = map calc_p freq in
      ((~.1. -~ p) *~ in_proc) +~ (p *~ last))

(* https://www.w3.org/2011/audio/audio-eq-cookbook.html *)
let biquad x a0 a1 a2 b0 b1 b2 =
  recursive 0.0 (fun y ->
      (b0 /~ a0 *~ x)
      +~ (b1 /~ a0 *~ del1 0. x)
      +~ (b2 /~ a0 *~ del1 0. (del1 0. x))
      -~ (a1 /~ a0 *~ y)
      -~ (a2 /~ a0 *~ del1 0. y))

let biquad_static x a0 a1 a2 b0 b1 b2 =
  let b0a0 = b0 /. a0 in
  let b1a0 = b1 /. a0 in
  let b2a0 = b2 /. a0 in
  let a1a0 = a1 /. a0 in
  let a2a0 = a2 /. a0 in
  recursive 0.0 (fun y ->
      (~.b0a0 *~ x)
      +~ (~.b1a0 *~ del1 0. x)
      +~ (~.b2a0 *~ del1 0. (del1 0. x))
      -~ (~.a1a0 *~ y)
      -~ (~.a2a0 *~ del1 0. y))

let blpf f q x =
  let w = ~.two_pi *~ (f /~ ~.(!sample_rate)) in
  let a = map sin w /~ (q *~ ~.2.) in
  let cosw = map cos w in
  let b1 = ~.1. -~ cosw in
  let b0 = b1 /~ ~.2. in
  let b2 = b0 in
  let a0 = ~.1. +~ a in
  let a1 = ~.(-2.) *~ cosw in
  let a2 = ~.1. -~ a in
  biquad x a0 a1 a2 b0 b1 b2

let blpf_static f q x =
  let w = two_pi *. (f /. !sample_rate) in
  let a = sin w /. (q *. 2.) in
  let cosw = cos w in
  let b1 = 1. -. cosw in
  let b0 = b1 /. 2. in
  let b2 = b0 in
  let a0 = 1. +. a in
  let a1 = -2. *. cosw in
  let a2 = 1. -. a in
  biquad_static x a0 a1 a2 b0 b1 b2

(* TODO dynamic version *)
let bhpf_static f q x =
  let w = two_pi *. (f /. !sample_rate) in
  let a = sin w /. (q *. 2.) in
  let cosw = cos w in
  let b0 = (1. +. cosw) /. 2. in
  let b1 = (1. +. cosw) *. -1. in
  let b2 = b0 in
  let a0 = 1. +. a in
  let a1 = -2. *. cosw in
  let a2 = 1. -. a in
  biquad_static x a0 a1 a2 b0 b1 b2

(* TODO dynamic version *)
let bbpf_static f q x =
  let w = two_pi *. (f /. !sample_rate) in
  let a = sin w /. (q *. 2.) in
  let cosw = cos w in
  let b0 = a in
  let b1 = 0. in
  let b2 = a *. -1. in
  let a0 = 1. +. a in
  let a1 = -2. *. cosw in
  let a2 = 1. -. a in
  biquad_static x a0 a1 a2 b0 b1 b2

(* Analysis  *)

let rms in_proc freq = map sqrt (lpf1 (map (fun x -> x *. x) in_proc) freq)

(* Audio Files  *)

(* deal with multiple channels *)
let sndfile fname =
  let buf = Sndfile.snd_read fname in
  let length = Bigarray.Array1.dim buf in
  map (fun i -> buf.{i mod length}) (inc_int 0 1)

(* Noise  *)

let rec rnd () = Cons (Random.float 2.0 -. 1.0, rnd)

(* Oscillators  *)

let sinosc freq =
  let phinc = calc_ph_inc !sample_rate in
  map sin (integrate (map (fun f -> f *. phinc) freq))

let fm_feedback (fac1, fac2) (offset1, offset2) =
  recursive (0.0, 0.0) (fun str ->
      zip pair
        (sinosc ((map snd str *~ fac1 *~ offset2) +~ offset1))
        (sinosc ((map fst str *~ fac2 *~ offset1) +~ offset2)))

let calc_diffs phases =
  let n = float_of_int @@ List.length phases in
  List.map
    (fun thisPh ->
      List.fold_left (fun diff ph -> diff +. sin (ph -. thisPh)) 0.0 phases /. n)
    phases

let kuramoto init coupling incr =
  recursive init (fun phases ->
      zip
        (List.map2 (fun offset old -> mod_float (offset +. old) two_pi))
        (map
           (fun ph ->
             List.map (fun p -> (p *. coupling) +. incr) (calc_diffs ph))
           phases)
        phases)

let impulse ph =
  zip
    (fun cur last ->
      if last > cur && abs_float (last -. cur) +. last > two_pi then 1.0
      else 0.0)
    ph (del1 0.0 ph)

(* TODO
 * sort and document functions in this module
 * analyis
 * memory delay
   * load soundfile
   * write soundfile
   * buffer read write
   * delayc
 * synth
   * fm
 * dynsys
   * hopf
   * kaneko 
   * compander
 * panning: channels 
   * more multichannel expansion
   * accesing, switching
 * filter
   * lagging
   * filterbank
 * data/utility
   * cycle
   * seq (casper)
   * muladd
   * gate
   * sample and hold
 * non-realtime
   * render to disk
   * non rt analysis tools, (like sc "signal" class)
 *)

(* Done
 * bpf, hpf, lpf
 * splay
 * rms
 * lpf
 * pan2
 * kuramoto
 * dirac
 * sinosc
 * phasor
 * infix operators
 * audio test
 * delay1
 * recursion
 * multichannel jack
 * compare rec fm to sc implementation: 
   high values lead to differences (double vs float issue ??)
 *)
