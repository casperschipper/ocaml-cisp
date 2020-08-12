open Seq

(* Seq is a thunk that when forced returns a value and a thunk to get the tail *)

(* 
this is the same as 
let thunk x = fun () -> x 
 *)

let samplerate = ref 44100.0

let thunk x () = x

let emptySt () = Nil

let force l = l ()

let ( |> ) x f = f x

let ( <| ) f x = f x

let ( >> ) f g x = g (f x)

let ( << ) f g x = f (g x)

let fst (x, _) = x

let snd (_, x) = x

let flip f a b = f b a

let ofInt = Float.of_int

let ofFloat = Int.of_float

let rec length sq = match sq () with Nil -> 0 | Cons (_, tl) -> 1 + length tl

let fromBinary b =
  let rec aux b base =
    if b = 0 then 0 else (b mod 10 * base) + aux (b / 10) (base * 2)
  in
  aux b 1

let rec append a b () =
  match a () with Nil -> b () | Cons (h, ls) -> Cons (h, append ls b)

(*
let rec appendSeq a b =
  match a () with
  | Nil -> b
  | Cons (this_a, rest) -> fun () -> Cons (this_a, appendSeq rest b)

let rec appendAlt a b () =
  match a () with
  | Nil -> b ()
  | Cons (h, tl) -> fun () -> Cons(h, appendAlt tl b)
*)

let rec cycle a () =
  let rec cycle_append current_a () =
    match current_a () with
    | Nil -> cycle a ()
    | Cons (this_a, rest) -> Cons (this_a, cycle_append rest)
  in
  cycle_append a ()

let rec range a b () = if a >= b then Nil else Cons (a, range (a +. 1.0) b)

(* not sure about these *)
let head ll = match ll () with Nil -> None | Cons (h, _) -> Some h

let tail ll = match ll () with Nil -> None | Cons (_, tl) -> Some (tl ())

(* this is not the same as ofList, but should be! 
let from_list list () =
  List.fold_right (fun x acc -> Cons (x, acc |> thunk)) list Nil
  *)

let ofList l =
  let rec aux l () = match l with [] -> Nil | x :: l' -> Cons (x, aux l') in
  aux l

let rec toList ls =
  match ls () with Nil -> [] | Cons (h, ts) -> h :: toList ts

let rec take n lst () =
  if n <= 0 then Nil
  else match lst () with Nil -> Nil | Cons (h, ts) -> Cons (h, take (n - 1) ts)

let for_example lst = lst |> take 40 |> toList

let rec drop n lst () =
  if n <= 0 then lst ()
  else match lst () with Nil -> Nil | Cons (_, tail) -> drop (n - 1) tail ()

let rec nth n sq = if n = 0 then head sq else nth (n - 1) sq

let reverse lst =
  let rec aux acc arg () =
    match arg () with
    | Nil -> acc
    | Cons (h, ts) -> aux (Cons (h, thunk acc)) ts ()
  in
  aux Nil lst

let rec split n lst =
  if n <= 0 then (thunk Nil, lst)
  else
    match lst () with
    | Nil -> (thunk Nil, thunk Nil)
    | Cons (x, xs) ->
        let f, l = split (n - 1) xs in
        (thunk (Cons (x, f)), l)

let rec filter f lst () =
  match lst () with
  | Nil -> Nil
  | Cons (h, tl) ->
      if f h then Cons (h, fun () -> filter f tl ()) else filter f tl ()

let rec foldr f z ll =
  match ll () with Nil -> z | Cons (h, tl) -> f h (foldr f z tl)

let rec fold_right f s acc =
  match s () with Nil -> acc | Cons (e, s') -> f e (fold_right f s' acc)

let rec concat str () =
  match str () with
  | Nil -> Nil
  | Cons (h, ls) -> (
    match h () with
    | Cons (h', ls') ->
        let newtail () = Cons (ls', ls) in
        Cons (h', concat newtail)
    | Nil -> concat ls () )

let concatMap f sq () = map f sq |> concat

let hd lst =
  match lst () with
  | Nil -> raise (Invalid_argument "empty list has no head")
  | Cons (h, _) -> h

let tl lst =
  match lst () with
  | Nil -> raise (Invalid_argument "empty list has no tail")
  | Cons (_, tl) -> tl

let for_all f sq =
  let rec aux sq start =
    match sq () with
    | Nil -> true
    | Cons (h, tl) -> if f h then aux tl start else false
  in
  aux sq true

let is_empty sq = match sq () with Nil -> true | _ -> false

let has_more sq = is_empty sq |> not

let foldHeads x acc =
  match x () with Nil -> acc | Cons (h, _) -> Cons (h, fun () -> acc)

let headsOfStreams sq () = fold_right foldHeads sq Nil

let foldTails x acc =
  match x () with Nil -> acc | Cons (_, ts) -> Cons (ts, fun () -> acc)

let tailsOfStreams sq () = fold_right foldTails sq Nil

let rec transpose sq () =
  match sq () with
  | Nil -> Nil
  | Cons (sqs, sqss) -> (
    match sqs () with
    | Cons (x, xs) ->
        Cons
          ( (fun () -> Cons (x, headsOfStreams sqss))
          , transpose <| thunk (Cons (xs, tailsOfStreams sqss)) )
    | Nil -> transpose sqss () )

let rec transpose_list lst =
  let foldHeads acc x = match x with [] -> acc | h :: _ -> h :: acc in
  let foldTails acc x = match x with [] -> acc | _ :: ts -> ts :: acc in
  match lst with
  | [] -> []
  | [] :: xss -> transpose_list xss
  | (x :: xs) :: xss ->
      (x :: List.fold_left foldHeads [] xss)
      :: transpose_list (xs :: List.fold_left foldTails [] xss)

let rec st a () =
  (* static *)
  Cons (a, st a)

let rec countFrom n () = Cons (n, countFrom (n + 1))

let count = countFrom 0

let rec zip a b () =
  match a () with
  | Nil -> Nil
  | Cons (a, atl) -> (
    match b () with Nil -> Nil | Cons (b, btl) -> Cons ((a, b), zip atl btl) )

let rec zipList a b =
  match a with
  | [] -> []
  | x :: xs -> ( match b with [] -> [] | y :: ys -> (x, y) :: zipList xs ys )

let rec zip3 a b c () =
  match (a (), b (), c ()) with
  | Cons (ha, lsa), Cons (hb, lsb), Cons (hc, lsc) ->
      Cons ((ha, hb, hc), zip3 lsa lsb lsc)
  | _ -> Nil

let rec zipWith f a b () =
  match a () with
  | Nil -> Nil
  | Cons (a, atl) -> (
    match b () with
    | Nil -> Nil
    | Cons (b, btl) -> Cons (f a b, zipWith f atl btl) )

let ( +~ ) = zipWith (fun a b -> a +. b)

let ( *~ ) = zipWith (fun a b -> a *. b)

let ( /~ ) = zipWith (fun a b -> a /. b)

let ( -~ ) = zipWith (fun a b -> a -. b)

let mixList lst () = List.fold_left ( +~ ) lst

(* zipped list will be lenght of shortest list *)

let clip low high x = if x < low then low else if x > high then high else x

let wrapf low high x =
  let range = low -. high |> abs_float in
  let modded = mod_float (x -. low) range in
  if modded < 0.0 then high +. x else low +. x

let wrap low high x =
  let range = low - high |> abs in
  let modded = (x - low) mod range in
  if modded < 0 then high + x else low + x

let rec walk start steps () =
  match steps () with
  | Cons (h, ls) ->
      let next = start +. h in
      Cons (start, walk next ls)
  | Nil -> Nil

(* operator is a function 
   to get the next value *)
let rec boundedFuncWalk start steps operator wrapfunc () =
  match steps () with
  | Cons (h, ls) ->
      let next = operator start h in
      Cons (wrapfunc start, boundedFuncWalk next ls operator wrapfunc)
  | Nil -> Nil

let boundedWalk start steps wrapfunc =
  let rec aux start steps () =
    match steps () with
    | Nil -> Nil
    | Cons (h, ls) ->
        let next = wrapfunc (start + h) in
        Cons (start, aux next ls)
  in
  aux start steps

let boundedWalkf start steps wrapfunc =
  let rec aux start steps () =
    match steps () with
    | Nil -> Nil
    | Cons (h, ls) ->
        let next = start +. h in
        Cons (wrapfunc start, aux next ls)
  in
  aux start steps

let safeIdx len idx = if idx < 0 then len - (abs idx mod len) else idx mod len

let indexArr arr idx =
  let len = Array.length arr in
  arr.(safeIdx len idx)

let rec index arr indexer () =
  match indexer () with
  | Nil -> Nil
  | Cons (idx, idxs) -> Cons (indexArr arr idx, index arr idxs)

let listWalk arr step () =
  let wrapFunc = wrap 0 (Array.length arr) in
  index arr (boundedWalk 0 step wrapFunc)

type 'a weightList = Weights of (int * 'a) list

let mkWeights lst = match lst with [] -> None | w -> Some (Weights w)

let weights weightLst () =
  let sumWeights = List.fold_left (fun acc (_, w) -> w + acc) 0 weightLst in
  let rec lookupWeight lst curr pick =
    match lst with
    | [] -> raise <| Invalid_argument "weight not found"
    | [(value, _)] -> value
    | (value, weight) :: ws ->
        let nextCurr = weight + curr in
        if pick < nextCurr then value else lookupWeight ws nextCurr pick
  in
  let rec aux weights max () =
    Cons (lookupWeight weights 0 (Random.int max), aux weights max)
  in
  aux weightLst sumWeights ()

let rec sometimes x y p () =
  let head () =
    let rnd = Random.int p in
    if rnd < 1 then y else x
  in
  Cons (head (), sometimes x y p)

let linInterp xa xb px = ((xb -. xa) *. px) +. xa

let rec indexLin arr indexer () =
  let len = Array.length arr in
  match indexer () with
  | Nil -> Nil
  | Cons (idx, idxs) ->
      let xa = idx |> Float.floor |> Int.of_float in
      let xb = (xa + 1) mod len in
      let xp = idx -. Float.of_int xa in
      let y = linInterp (indexArr arr xa) (indexArr arr xb) xp in
      Cons (y, indexLin arr idxs)

let sineseg wavesamps =
  let incr = 1.0 /. Float.of_int wavesamps in
  let f x = 2.0 *. Float.pi *. x |> sin in
  let rec aux x () = if x >= 1.0 then Nil else Cons (f x, aux (x +. incr)) in
  aux 0.0

let arr_of_seq str = Array.of_seq str

(* TODO
let rec iterates start fs =
  match fs () with Nil -> Nil | Cons (h, ls) -> start () fs *)

let rec repeat n x () = if n > 0 then Cons (x, repeat (n - 1) x) else Nil

let hold repeats source =
  let ctrl = zip repeats source in
  map (fun (n, src) -> repeat n src) ctrl |> concat

let embed str () = Cons (str, thunk Nil) (* create a singleton stream *)

(* returnes the loops and the tail of the source
   preferably source is infinite *)
let oneLoop size n str () =
  let snippet, tail = split size str in
  (snippet |> repeat n |> concat, tail)

let loop size n src =
  let control = zip size n in
  let rec loops ctrl src () =
    match ctrl () with
    | Nil -> Nil
    | Cons ((size, num), nextCtrl) ->
        let currLoop, rest = oneLoop size num src () in
        Cons (currLoop, loops nextCtrl rest)
  in
  loops control src

let trunc = map Int.of_float

let floatify = map Float.of_int

let rvfi low high =
  let range = abs_float (low -. high) in
  let offset = min low high in
  Random.float range +. offset

let rvi low high =
  let range = abs (low - high) in
  let offset = min low high in
  Random.int range + offset

let rv low high =
  let control = zip low high in
  map (fun (l, h) -> rvi l h) control

let rvf low high =
  let control = zip low high in
  map (fun (l, h) -> rvfi l h) control

(* choice *)
let ch arr =
  let picker = rv (st 0) (st (Array.length arr)) in
  index arr picker

let mtof midi = 440.0 *. (2.0 ** ((midi -. 69.0) /. 12.0))

let ftom freq = (12.0 *. log (freq /. 440.0)) +. 69.0

let rec collatz n () =
  if n = 1 then Nil (* lets end it here, normally loops 1 4 2 1 4 2.. *)
  else
    let even x = x mod 2 = 0 in
    let next = if even n then n / 2 else (n * 3) + 1 in
    Cons (next, collatz next)

(* use floats as arguments to somethign that expects streams *)
let lift f a b = f (st a) (st b)

let pair a b = (a, b)

let lineSegment curr target n () =
  let rate = (target -. curr) /. n in
  let reachedEnd =
    if rate > 0.0 then fun x -> x >= target else fun x -> x <= target
  in
  let rec segment curr () =
    if reachedEnd curr then Nil else Cons (curr, segment (curr +. rate))
  in
  segment curr

(* [0,1];[1,2];[2,3] etc...  *)
let rec chain start str () =
  match str () with
  | Nil -> Nil
  | Cons (h, ls) -> Cons (pair start h, chain h ls)

let selfChain str () =
  (* (a,b) (b,c) (c,d) (d,e) etc... *)
  match str () with Nil -> Nil | Cons (h, ls) -> chain h ls ()

let seq lst = lst |> ofList |> cycle

let line target n =
  let control = zip (selfChain target) n in
  map (fun ((a, b), n') -> lineSegment a b n' ()) control |> concat

(* audio only, linear interpolation *)
let mkDel max del src () =
  let delay = Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout max in
  let index = map (fun x -> x mod max) count in
  let parameters = zip index src in
  let wr = map (fun (idx, src) -> delay.{idx} <- src) parameters in
  let readParameters = zip3 index del wr in
  map
    (fun (idx, del, wr) ->
      let maxf = ofInt max in
      let idxf = ofInt idx in
      let later = mod_float (maxf +. idxf -. del) maxf in
      let x0_idx = Int.of_float later in
      let x0 = delay.{x0_idx} in
      let x1 = delay.{(x0_idx + 1) mod max} in
      let xp = later -. Float.of_int x0_idx in
      let value = linInterp x0 x1 xp in
      wr ; value)
    readParameters

let genSine size =
  let twopi = 2.0 *. Float.pi in
  let gen i = ofInt i /. ofInt size *. twopi |> sin in
  Array.init size gen

let waveOsc arr frq =
  let arraySize = Array.length arr |> Float.of_int in
  let incr = frq /. !samplerate *. arraySize in
  let phasor = walk 0.0 (st incr) in
  index arr (trunc <| phasor)

let waveOscL arr frq =
  let arraySize = Array.length arr |> Float.of_int in
  let incr = frq /. !samplerate *. arraySize in
  let phasor = walk 0.0 (st incr) in
  indexLin arr phasor
