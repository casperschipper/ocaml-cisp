(* Global settings *)

let output_buffer_size = 32 (* HAS TO BE A POWER OF TWO *)

let sample_rate = ref 44100.

let input_array = Array.create_float 64

let input_channels = ref 0

(* Numerical globals *)

let pi = 4.0 *. atan 1.0

let two_pi = 2.0 *. pi

(* Utility functions *)
let pair a b = (a, b)

let calc_ph_inc sr = 2.0 *. pi /. sr

let rec mkLstN f i = if i <= 0 then [] else f i :: mkLstN f (i - 1)

(* Process type *)

let empty_state = [||]

type sample_counter = int

(* closure with a mutable value, tuple empty state header  *)

(* ; func: float array -> int -> float array * 'output

input, counter, output *)

type 'output t =
  { mutable state: float array
  ; func: float array -> int -> float array * 'output
  ; mutable last_output_idx: sample_counter
  ; mutable outputs: 'output array }

let mk init_state func init_out =
  { state= init_state
  ; func
  ; last_output_idx= 0
  ; outputs= Array.make output_buffer_size init_out }

let mk_no_state func init_out =
  { state= empty_state
  ; func
  ; last_output_idx= 0
  ; outputs= Array.make output_buffer_size init_out }

(* let inc_indices g =
 *   match g.last_output_idx with
 *   | counter, idx ->
 *       g.last_output_idx <- (counter + 1, (idx + 1) mod output_buffer_size) *)

let last_value g =
  g.outputs.(g.last_output_idx land (output_buffer_size - 1))

let idx i =
  if i < 0 then output_buffer_size - 1 + i else i land (output_buffer_size - 1)

let previous_value g i =
  let last_sc = g.last_output_idx in
  let arr_idx = last_sc land (output_buffer_size - 1) in
  g.outputs.(idx (arr_idx + (i - last_sc) - 1))

let generate_next g =
  let () = g.last_output_idx <- g.last_output_idx + 1 in
  let counter = g.last_output_idx in
  let new_state, output = g.func g.state counter in
  g.state <- new_state ;
  (* g.outputs.(counter mod output_buffer_size) <- output ; *)
  g.outputs.(counter land (output_buffer_size - 1)) <- output ;
  output

let value_at g i =
  let last_sc = g.last_output_idx in
  let arr_idx = last_sc land (output_buffer_size - 1) in
  while g.last_output_idx < i do
    let _ = generate_next g in
    ()
  done ;
  g.outputs.(idx (arr_idx + (i - last_sc)))

let generate g i =
  while g.last_output_idx < i do
    let _ = generate_next g in
    ()
  done ;
  last_value g

let rec toLst n g =
  if n <= 0 then []
  else
    let this = last_value g in
    let _ = generate_next g in
    this :: toLst (n - 1) g

let rec toSeq g () =
  let open Seq in
  Cons (generate_next g, toSeq g)

(* takes a Seq t and makes it into process.t, ignores input.
     the lazy tail of the Seq, is being stored in a mutable variable within a closure 
     so the function that is returned behaving as a 'consumable'.
   *)

let ofSeq sq =
  let open Seq in
  let state = ref sq in
  let f _ _ =
    match !state () with
    | Cons (s, tl) ->
        state := tl ;
        (empty_state, s)
    | Nil -> (empty_state, 0.0)
  in
  mk_no_state f 0.0

let const v = mk empty_state (fun _ _ -> (empty_state, v)) v

let ( ~. ) = const

(* Applying functions *)

let map f g =
  mk_no_state (fun _ i -> (empty_state, f (value_at g i))) (f (last_value g))

let zip f g1 g2 =
  { state= empty_state
  ; func= (fun _ i -> (empty_state, f (value_at g1 i) (value_at g2 i)))
  ; last_output_idx= 0
  ; outputs= Array.make output_buffer_size (f (last_value g1) (last_value g2))
  }

(* Arithmetic operations *)

let add g1 g2 =
  { state= empty_state
  ; func= (fun _ i -> (empty_state, value_at g1 i +. value_at g2 i))
  ; last_output_idx= 0
  ; outputs= Array.make output_buffer_size (last_value g1 +. last_value g2) }

let sum a = List.fold_left add (const 0.0) a

let ( +~ ) a b = zip ( +. ) a b

let ( *~ ) a b = zip ( *. ) a b

let ( -~ ) a b = zip ( -. ) a b

let ( /~ ) a b = zip ( /. ) a b

let ( **~ ) a b = zip ( ** ) a b

(* Audio input *)

let input c =
  mk_no_state (fun _ _ -> (empty_state, input_array.(c))) input_array.(c)

(* Delays *)

let del1 g =
  { state= empty_state
  ; func= (fun _ i -> (empty_state, value_at g (i - 1)))
  ; last_output_idx= 0
  ; outputs= Array.make output_buffer_size (last_value g) }

(* Increment and integration *)

(* let integrate start g =
 *   { state= 0.
 *   ; func=
 *       (fun s i ->
 *         let incr = value_at g i in
 *         let out = s +. incr in
 *         (out, out))
 *   ; last_output_idx= (0, 0)
 *   ; outputs= Array.make output_buffer_size start } *)

let inc start increment =
  mk [|start|]
    (fun s _ ->
      let out = s.(0) +. increment in
      ([|out|], out))
    start

let inc_int start increment =
  let incr = float_of_int increment in
  mk
    [|float_of_int start|]
    (fun s _ ->
      let out = s.(0) +. incr in
      ([|out|], int_of_float out))
    start

(* Recursion *)

let from_ref r = mk_no_state (fun _ _ -> (empty_state, !r)) !r

let recursive start_value proc =
  let current = ref start_value in
  let rec_seq =
    map
      (fun this_value ->
        let () = current := this_value in
        this_value)
      (proc (from_ref current))
  in
  rec_seq

let integrate increment = recursive 0.0 (fun last -> increment +~ last)

(* Noise *)

let rnd =
  mk empty_state
    (fun _ _ -> (empty_state, Random.float 2.0 -. 1.0))
    (Random.float 2.0 -. 1.0)

(* Oscillators *)

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
    ph (del1 ph)

(* Filters *)

let calc_p freq = 1. -. (2. *. tan (freq /. !sample_rate))

let lpf1_static in_proc freq =
  let p = calc_p freq in
  mk [|0.|]
    (fun last i ->
      let out = ((1. -. p) *. value_at in_proc i) +. (p *. last.(0)) in
      ([|out|], out))
    0.

(* https://www.w3.org/2011/audio/audio-eq-cookbook.html *)
let biquad_static x a0 a1 a2 b0 b1 b2 =
  let b0a0 = b0 /. a0 in
  let b1a0 = b1 /. a0 in
  let b2a0 = b2 /. a0 in
  let a1a0 = a1 /. a0 in
  let a2a0 = a2 /. a0 in
  { state= [|0.; 0.; 0.; 0.|]
  ; func=
      (fun s i ->
        let this_x = value_at x i in
        let x1 = s.(0) in
        let x2 = s.(1) in
        let y1 = s.(2) in
        let y2 = s.(3) in
        let new_y =
          (this_x *. b0a0) +. (b1a0 *. x1) +. (b2a0 *. x2) -. (a1a0 *. y1)
          -. (a2a0 *. y2)
        in
        ([|this_x; x1; new_y; y1|], new_y))
  ; last_output_idx= 0
  ; outputs= Array.make output_buffer_size 0. }

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

let rec map_succ f = function
  | a :: b :: r -> f a b :: map_succ f (b :: r)
  | _ -> []

let rec geo_series n start f =
  if n < 2 then [start] else start :: geo_series (n - 1) (start *. f) f

let geo_from_to n from to_f =
  let fac = (to_f /. from) ** (1. /. float_of_int (n - 1)) in
  geo_series n from fac

let min_sub_freq f1 f2 = min f1 (f2 -. f1)

let min_sub_freqs freqs = map_succ min_sub_freq freqs

let casc2_band f1 f2 q x =
  bhpf_static f1 q (bhpf_static f1 q (blpf_static f2 q (blpf_static f2 q x)))

let casc_bank q n start_f end_f x =
  map_succ (fun f1 f2 -> casc2_band f1 f2 q x) (geo_from_to n start_f end_f)

(* let rec keep_last = function [] -> [] | [a] -> [a] | _ :: r -> keep_last r *)

let fbank_subtract lp_filter_fun q n start_f end_f x =
  let bands =
    List.fold_left
      (fun (bands, total_signal) f ->
        let this_band = lp_filter_fun f q total_signal in
        (this_band :: bands, total_signal -~ this_band))
      ([], x)
      (geo_from_to n start_f end_f)
  in
  fst bands @ [snd bands]

let fbank_map filter_fun q n start_f end_f x =
  List.map (fun f -> filter_fun f q x) (geo_from_to n start_f end_f)

(* Analysis  *)

let rms freq in_proc =
  map sqrt (lpf1_static (map (fun x -> x *. x) in_proc) freq)

(* Multichannel  *)

let evert lst_proc =
  let n = List.length (last_value lst_proc) in
  mkLstN (fun i -> map (fun l -> List.nth l (n - i)) lst_proc) n

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

let split str = [map fst str; map snd str]

let ( |>> ) lst1 lst2 = List.map2 (fun s1 s2 -> s1 |> s2) lst1 lst2

let ( ||> ) lst f = List.map (fun s -> f s) lst
