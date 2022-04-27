(* explicitely infinite seq, never ends! *)
type 'a inf_node =
  InfCons of 'a * 'a t
and +'a t =
  unit -> 'a inf_node

let rec countFrom n () =
  InfCons(n, countFrom (n + 1))

let rec map f isq () =
  match isq () with
    InfCons(h,tl) -> InfCons(f h,map f tl)

let rec repeat x () =
  InfCons(x, repeat x)

let head sq =
  match sq () with
    InfCons(x,_) -> x

let tail sq =
  match sq () with
    InfCons(_,ts) -> ts
          
(* use sparingly ! *)
let rec toSeq infSq () = 
  match infSq () with
  | InfCons(h,tl) -> Seq.Cons(h,toSeq tl)

let to_seq =
  toSeq
 

(* get a finite chunk *)
let rec take n isq () =
  if n <= 0 then
    Seq.Nil
  else
    let (InfCons (h, tl)) = isq () in
    Seq.Cons(h,take (n-1) tl)

let rec drop n isq =
  if n <= 0 then 
    isq
  else
    match isq () with
    | InfCons(_,tail) -> drop (n - 1) tail

let rec chunk chunk_sizes input =
  match chunk_sizes () with
  | InfCons(size, rest) ->
    InfCons(take size input, fun () -> chunk rest (drop size input))


(* flatten infinite stream of finite streams *)
let rec concatSq ssq () =
  match ssq () with 
    InfCons (h, ls) -> (
    match h () with
    | Seq.Cons (h', ls') ->
       let newtail () = InfCons (ls', ls) in
       InfCons (h', concatSq newtail)
    | Seq.Nil -> concatSq ls () )

(* for mapping multiargument functions *)
let rec andMap sqa sqf () =
      match (sqa (),sqf ()) with
      | (InfCons(x,xs), InfCons(f,fs)) ->
        InfCons (f x, andMap xs fs)
    
        
let map2 f sqa sqb =
  map f sqa |> andMap sqb

let hold repetitions source =
  let f src n =
    src |> repeat |> take n
  in
  map2 f source repetitions |> concatSq

let cycleSq sq =
  repeat sq |> concatSq

let seq lst =
  lst |> List.to_seq |> cycleSq

let rec unfold f seed () =
  let (current, nextSeed) = f seed in
  InfCons(current,unfold f nextSeed)

let uncons isq =
  match isq () with
  | InfCons(current,tl) -> (current, tl)

let recursive control init update eval =
  let rec aux cSq state () =
    match cSq () with
    InfCons(c,tl) ->
     let nxt =
       update c state
     in
     InfCons(eval state, aux tl nxt)
  in
  aux control init

let recursive1 control init update eval =
  let rec aux cSq state () =
    match cSq () with
    InfCons(c,tl) ->
     let nxt =
       update c state
     in
     InfCons(eval nxt, aux tl nxt)
  in
  aux control init

let walki (start: int) steps =
  recursive
    steps
    start
    ( + ) 
    (fun x -> x)
    

let walk (start: float) steps =
  recursive steps start ( +. ) (fun x -> x)

    
let chunk chunk_size input =
  let init = (chunk_size, input) in
  recursive 


let rec sometimes x y p () =
  let fst () =
    let rnd = Random.int p in
    if rnd < 1 then y else x
  in
  InfCons (fst (), sometimes x y p)

  

let rec zip sqa sqb () =
  match (sqa (),sqb ()) with
    (InfCons (x,xs),InfCons(y,ys)) ->
    InfCons ((x,y),zip xs ys)

let rec zipWith f sqa sqb () =
  match (sqa (),sqb ()) with
    (InfCons (x,xs),InfCons(y,ys)) ->
    InfCons (f x y,zipWith f xs ys)


let applySq fSq sq = zip fSq sq |> map (fun (f,x ) -> f x)





