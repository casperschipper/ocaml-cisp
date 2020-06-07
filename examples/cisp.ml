open Seq

(* Seq is a thunk that when forced returns a value and a thunk to get the tail *)

(* 
this is the same as 
let thunk x = fun () -> x 
 *)

let thunk x () = x

let force l = l ()

let ( |> ) x f = f x

let fst (x, _) = x

let snd (_, x) = x

let rec append a b () =
  match a () with Nil -> b | Cons (h, ls) -> Cons (h, append ls b)

let rec append_seq a b =
  match a () with
  | Nil -> b
  | Cons (this_a, rest) -> fun () -> Cons (this_a, append_seq rest b)

let rec cycle a () =
  let rec cycle_append current_a () =
    match current_a () with
    | Nil -> cycle a ()
    | Cons (this_a, rest) -> Cons (this_a, cycle_append rest)
  in
  cycle_append a ()

let rec range a b () = if a >= b then Nil else Cons (a, range (a +. 1.0) b)

(* not sure about these *)
let head ll = match ll with Nil -> None | Cons (h, _) -> Some h

let tail ll = match ll with Nil -> None | Cons (_, tl) -> Some (tl ())

let rec nth ll n =
  if n < 0 then None
  else if n = 0 then head ll
  else Option.bind (tail ll) (fun staart -> nth staart (n - 1))

(* this is not the same as of_list, but should be! 
let from_list list () =
  List.fold_right (fun x acc -> Cons (x, acc |> thunk)) list Nil
  *)

let of_list l =
  let rec aux l () = match l with [] -> Nil | x :: l' -> Cons (x, aux l') in
  aux l

let rec to_list ls =
  match ls () with Nil -> [] | Cons (h, ts) -> h :: to_list ts

let rec take n lst () =
  if n <= 0 then Nil
  else match lst () with Nil -> Nil | Cons (h, ts) -> Cons (h, take (n - 1) ts)

let for_example lst = lst |> take 40 |> to_list

let rec drop n lst () =
  if n <= 0 then lst ()
  else match lst () with Nil -> Nil | Cons (_, tail) -> drop (n - 1) tail ()

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

let rec filter f lst =
  match lst () with
  | Nil -> Nil
  | Cons (h, tl) -> if f h then Cons (h, fun () -> filter f tl) else filter f tl

let rec foldr f z ll =
  match ll () with Nil -> z | Cons (h, tl) -> f h (foldr f z tl)

let rec fold_right f s acc =
  match s () with Nil -> acc | Cons (e, s) -> f e (fold_right f s acc)

let rec concat str () =
  match str () with
  | Nil -> Nil
  | Cons (h, ls) -> (
    match h () with
    | Cons (h', ls') ->
        let newtail () = Cons (ls', ls) in
        Cons (h', concat newtail)
    | Nil -> concat ls () )

let hd lst =
  match lst () with
  | Nil -> raise (Invalid_argument "empty list has no head")
  | Cons (h, _) -> h

let tl lst =
  match lst () with
  | Nil -> raise (Invalid_argument "empty list has no tail")
  | Cons (_, tl) -> tl

let rec transpose lst () =
  let heads = map hd lst in
  let tails = map tl lst |> transpose in
  Cons (heads, tails)

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

let rec zip3 a b c () =
  match (a (), b (), c ()) with
  | Cons (ha, lsa), Cons (hb, lsb), Cons (hc, lsc) ->
      Cons ((ha, hb, hc), zip3 lsa lsb lsc)
  | _ -> Nil

(* zipped list will be lenght of shortest list *)

let rec walk start steps () =
  match steps () with
  | Cons (h, ls) ->
      let next = start +. h in
      Cons (start, walk next ls)
  | Nil -> Nil

let rec boundedWalk start steps wrap () =
  match steps () with
  | Nil -> Nil
  | Cons (h, ls) ->
      let next = start +. h in
      Cons (wrap start, boundedWalk next ls wrap)

let clip low high x = if x < low then low else if x > high then high else x

(* let rec wrapf *)

(* TODO
let rec iterates start fs =
  match fs () with Nil -> Nil | Cons (h, ls) -> start () fs *)

let rec repeat n x () = if n > 0 then Cons (x, repeat (n - 1) x) else Nil

let hold repeats source () =
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
  let rec loops crtl src () =
    match crtl () with
    | Nil -> Nil
    | Cons ((size, num), nextCtrl) ->
        let currLoop, rest = oneLoop size num src () in
        Cons (currLoop, loops nextCtrl rest)
  in
  loops control src

let trunc = map Int.of_float

let floatify = map Float.of_int

let rvfi low high () =
  let range = abs_float (low -. high) in
  let offset = min low high in
  Random.float range +. offset

let rvf low high () =
  let control = zip low high in
  map (fun (l, h) -> rvfi l h ()) control

let threeLists = [[11; 12; 13]; [42; 43; 44]; [100; 99]]

let seqs = List.map (fun lst -> of_list lst |> cycle) threeLists

let l_seqs = of_list seqs (* make this list also a lazy stream *)

let addCount () = Cons (count, l_seqs)

(* this adds in another infinite list, wich counts natural numbers *)

let trans = transpose addCount

let cat = trans |> concat |> take 30 |> to_list

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

let rec chain start str () =
  match str () with
  | Nil -> Nil
  | Cons (h, ls) -> Cons (pair start h, chain h ls)

let seq lst = lst |> of_list |> cycle

let selfChain str () =
  (* (a,b) (b,c) (c,d) (d,e) etc... *)
  match str () with Nil -> Nil | Cons (h, ls) -> chain h ls ()

let mkLine target n () =
  let control = zip (selfChain target) n in
  map (fun ((a, b), n') -> lineSegment a b n' ()) control |> concat

let foo = [11.0; 12.0; 13.0; 14.0] |> of_list

let testSah = lift rvf (-0.5) 0.5 () |> hold (trunc (lift rvf 1.0 1000.0 ()))

let a = lift rvf (-1.0) 1.0

let b = lift rvf 1.0 10.0

let myLine = mkLine (a ()) (b ())

let myLoopyLines =
  myLine ()
  |> loop (lift rvf 1.0 32.0 () |> trunc) (lift rvf 1.0 32.0 () |> trunc)
  |> concat

let _ =
  let proc = Process.ofSeq myLoopyLines in
  Jack.play 0 Process.sample_rate [proc]
