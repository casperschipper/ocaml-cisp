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

let rec toSeq infSq () = 
  match infSq () with
  | InfCons(h,tl) -> Seq.Cons(h,toSeq tl)

let rec take n isq () =
  if n <= 0 then
    Seq.Nil
  else
    let (InfCons (h, tl)) = isq () in
    Seq.Cons(h,take (n-1) tl)

let rec concatSq ssq () =
  match ssq () with 
    InfCons (h, ls) -> (
    match h () with
    | Seq.Cons (h', ls') ->
       let newtail () = InfCons (ls', ls) in
       InfCons (h', concatSq newtail)
    | Seq.Nil -> concatSq ls () )

let cycleSq sq =
  repeat sq |> concatSq

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
    





