(* explicitely infinite seq, never ends! *)
type 'a inf_node = InfCons of 'a * 'a t
and +'a t = unit -> 'a inf_node

let rec countFrom n () = InfCons (n, countFrom (n + 1))

let rec map f isq () =
  match isq () with InfCons (h, tl) -> InfCons (f h, map f tl)

let rec repeat x () = InfCons (x, repeat x)
let rec generator f () = InfCons (f (), generator f)
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
let ( <*> ) fab fa = andMap fa fab
let ( <$> ) f fa = map f fa

let hold repetitions source =
  let f src n = src |> repeat |> take n in
  map2 f source repetitions |> concatSq

let cycleSq sq = repeat sq |> concatSq
let seq lst = lst |> List.to_seq |> cycleSq
let of_list = seq

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

let rec zip sqa sqb () =
  match (sqa (), sqb ()) with
  | InfCons (x, xs), InfCons (y, ys) -> InfCons ((x, y), zip xs ys)

let rec zipWith f sqa sqb () =
  match (sqa (), sqb ()) with
  | InfCons (x, xs), InfCons (y, ys) -> InfCons (f x y, zipWith f xs ys)

let applySq fSq sq = zip fSq sq |> map (fun (f, x) -> f x)
let wrap = Toolkit.wrap

let index array indexer =
  let len = Array.length array in
  let f index = index |> wrap 0 len |> Array.get array in
  indexer |> map f

let index_seq (arr : 'a t Array.t) indexer =
  let array_size = Array.length arr in
  let unfolder (array, idx) =
    match idx () with
    | InfCons (firstIdx, restIdx) -> (
        let safeIdx = wrap 0 array_size firstIdx in
        let indexed = Array.get array safeIdx in
        match indexed () with
        | InfCons (current, tail) ->
            Array.set array safeIdx tail;
            (current, (array, restIdx)))
  in
  unfold unfolder (arr, indexer)

let rec sometimes x y p () =
  let head () =
    let rnd = if p < 1 then -1 else Random.int p in
    if rnd = 0 then y else x
  in
  InfCons (head (), sometimes x y p)

let ch_seq (arr : 'a t Array.t) =
  let len = Array.length arr in
  let f () = Toolkit.rvi 0 len in
  let indexer = generator f in
  index_seq arr indexer

let rec simpleRecursive init update evaluate () =
  let nextState = update init in
  InfCons (evaluate init, simpleRecursive nextState update evaluate)

let series_infSq arr =
  let lst = Array.to_list arr in
  match lst with
  | [] -> repeat (repeat 0.0)
  | x :: xs ->
      let update (current, _) =
        match current with
        | [] -> (
            let newList = Toolkit.shuffle lst in
            match newList with [] -> ([], repeat 0.0) | x :: xs -> (xs, x))
        | x :: xs -> (xs, x)
      in
      simpleRecursive (xs, x) update (fun (_, curX) -> curX)

let fst (x, _) = x
let snd (_, y) = y

let rec transpose (sqqss : 'a t Seq.t) () =
  let uncons =
    sqqss |> Seq.map (fun sq -> match sq () with InfCons (x, xs) -> (x, xs))
  in
  InfCons (Seq.map fst uncons, transpose (Seq.map snd uncons))

let rec self_chain sq () =
  match sq () with
  | InfCons (h1, tl1) -> (
      match tl1 () with InfCons (h2, _) -> InfCons ((h1, h2), self_chain tl1))

type 'a tLineState = {
  oldT : 'a Time.t;
  oldX : float;
  targetT : 'a Time.t;
  targetX : float;
  control : ('a Time.t * float) t;
}

let tline_start clock startX timeToNext sq =
  let evaluate { oldT; oldX; targetT; targetX; _ } =
    let now = clock () in
    if now = targetT then targetX
    else
      let segmentDur = Time.sub targetT oldT in
      let timeSinceOldT = Time.sub now oldT in
      let diffX = targetX -. oldX in
      let progress = Time.divt timeSinceOldT segmentDur |> max 0.0 |> min 1.0 in
      match Float.classify_float progress with
      | Float.FP_nan -> oldX
      | Float.FP_infinite -> oldX
      | _ -> oldX +. (progress *. diffX)
  in
  let rec updateControl now oldT ctrl =
    (* let _ = print_string "\n-----update control-----\n" in *)
    match ctrl () with
    | InfCons ((tDelta, newX), ctrl_tail) ->
        let newT = Time.add oldT tDelta in
        if newT >= now then ((newT, newX), ctrl_tail)
        else
          (* let _ = print_string "newT is not bigger than now: "; flush stdout in *)
          updateControl now newT ctrl_tail
  in
  let initial =
    let ctrl = zip timeToNext sq in
    let now = clock () in
    let (targetT_, targetX), ctrlTail = updateControl now now ctrl in
    {
      oldT = now;
      oldX = startX;
      targetT = targetT_;
      targetX;
      control = ctrlTail;
    }
  in
  let update state =
    let now = clock () in
    let newState =
      if state.targetT > now then state (* no changes *)
      else
        let (newT, newX), tail =
          updateControl now state.targetT state.control
        in
        {
          oldT = state.targetT;
          oldX = state.targetX;
          targetT = newT;
          targetX = newX;
          control = tail;
        }
    in
    (* print_tline_state newState ;  *)
    newState
  in
  unfold (fun state -> (evaluate state, update state)) initial

(* question: how to deal with in between values ? *)
