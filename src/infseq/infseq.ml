(* explicitely infinite seq, never ends! *)
type 'a inf_node = InfCons of 'a * 'a t
and +'a t = unit -> 'a inf_node

let rec countFrom n () = InfCons (n, countFrom (n + 1))

let rec map f isq () =
  match isq () with InfCons (h, tl) -> InfCons (f h, map f tl)

let rec repeat x () = InfCons (x, repeat x)

let rec generator f () =
  InfCons (f (), generator f)

let head sq = match sq () with InfCons (x, _) -> x
let tail sq = match sq () with InfCons (_, ts) -> ts

(* use sparingly ! *)
let rec toSeq infSq () =
  match infSq () with InfCons (h, tl) -> Seq.Cons (h, toSeq tl)

let to_seq = toSeq

(* get a finite chunk *)
let rec take n isq () =
  if n <= 0 then Seq.Nil
  else
    let (InfCons (h, tl)) = isq () in
    Seq.Cons (h, take (n - 1) tl)

let rec drop n isq =
  if n <= 0 then isq
  else match isq () with InfCons (_, tail) -> drop (n - 1) tail

(* flatten infinite stream of finite streams *)
let rec concatSq ssq () =
  match ssq () with
  | InfCons (h, ls) -> (
      match h () with
      | Seq.Cons (h', ls') ->
          let newtail () = InfCons (ls', ls) in
          InfCons (h', concatSq newtail)
      | Seq.Nil -> concatSq ls ())

(* for mapping multiargument functions *)
let rec andMap sqa sqf () =
  match (sqa (), sqf ()) with
  | InfCons (x, xs), InfCons (f, fs) -> InfCons (f x, andMap xs fs)

let map2 f sqa sqb = map f sqa |> andMap sqb

let hold repetitions source =
  let f src n = src |> repeat |> take n in
  map2 f source repetitions |> concatSq

let cycleSq sq = repeat sq |> concatSq
let seq lst = lst |> List.to_seq |> cycleSq

let rec unfold f seed () =
  let current, nextSeed = f seed in
  InfCons (current, unfold f nextSeed)

let uncons isq = match isq () with InfCons (current, tl) -> (current, tl)

let recursive control init update eval =
  let rec aux cSq state () =
    match cSq () with
    | InfCons (c, tl) ->
        let nxt = update c state in
        InfCons (eval state, aux tl nxt)
  in
  aux control init

let recursive1 control init update eval =
  let rec aux cSq state () =
    match cSq () with
    | InfCons (c, tl) ->
        let nxt = update c state in
        InfCons (eval nxt, aux tl nxt)
  in
  aux control init

let walki (start : int) steps = recursive steps start ( + ) (fun x -> x)
let walk (start : float) steps = recursive steps start ( +. ) (fun x -> x)

let rec chunk chunk_size input () =
  match chunk_size () with
  | InfCons (f, rest) ->
      let n = match int_of_float f with 0 -> 1 | nonZero -> nonZero in
      InfCons (take n input, chunk rest (drop n input))

let rec sometimes x y p () =
  let fst () =
    let rnd = Random.int p in
    if rnd < 1 then y else x
  in
  InfCons (fst (), sometimes x y p)

let rec zip sqa sqb () =
  match (sqa (), sqb ()) with
  | InfCons (x, xs), InfCons (y, ys) -> InfCons ((x, y), zip xs ys)

let rec zipWith f sqa sqb () =
  match (sqa (), sqb ()) with
  | InfCons (x, xs), InfCons (y, ys) -> InfCons (f x y, zipWith f xs ys)

let applySq fSq sq = zip fSq sq |> map (fun (f, x) -> f x)

let wrap low high x =
  let range = low - high |> abs in
  let modded = (x - low) mod range in
  if modded < 0 then high + x else low + x

let index array indexer =
  let len = Array.length array in
  let f index =
    index |> wrap 0 len |> Array.get array
  in
indexer |> map f 

let index_seq (arr : 'a t Array.t) indexer =
  let array_size = Array.length arr in
  let unfolder (array,idx) =
    match idx () with
    | InfCons(firstIdx, restIdx) ->
      let safeIdx = wrap 0 array_size firstIdx in
      let indexed = Array.get array safeIdx in
      match indexed () with
      | InfCons(current, tail) -> 
        Array.set array safeIdx tail;
         (current,(array,restIdx))
  in
  unfold unfolder (arr,indexer) 

let ch_seq (arr : 'a t Array.t) indexer =
  let rand_index = Array.length arr |> Cisp.rvi 0 
  index_seq arr (generator rand_index) 