open Cisp
open Midi

let ofOpt optEvt = match optEvt with Some evt -> evt | None -> SilenceEvent

let intervArr = [|7;4;4;7;3|] 

let writer =
  let index = wrappedCount intervArr in
  let generator = ch [|7;4;3;5|] in
  let wrf (i, v) = writeOne intervArr i v in
  zip index generator |> Seq.map wrf |> syncEffectClock (interval (rv (st 1) (st 10)))

let oneWalk start =
  let idx = wrappedCount intervArr in
  let steps = (index intervArr idx |> take 5) *~ (st (-1)) in
  walki start steps

type direction =
  Up
| Down

(*type walkmind direction is state *)
type dwalkState = {
    out : int
   ;dir : direction
  }

type posInt =
  Posi of int

let positive x =
  if x == Int.min_int then Posi 0 else
  Posi (abs x)
    
let sortArgs x y =
  if x > y then
    (y,x)
  else
    (x,y)

let sortArgsBy f x y =
  if f x y then
    (x,y)
  else
    (y,x)

let dwalk start steps boundary1 boundary2 =
  let safeSteps = Seq.map positive steps in
  let ctrlSq = zip3 safeSteps boundary1 boundary2 in
  let init = { out = start; dir = Up } in
  let update (Posi step,x,y) state =
    let prev = state.out in
    let (min,max) = sortArgs x y in
    let newDir =
      if state.out <= min then
        Up
      else
        if state.out >= max then
          Down
        else
          state.dir
    in
    let operator = match newDir with Up -> ( + ) | Down -> ( - ) in
    ({ out = operator prev step ; dir = newDir })
  in
  let eval state =
    state.out
  in  
  recursive1 ctrlSq init update eval

(* a walk that updates boundaries only when hit *)

let uncons sq =
  match sq () with
  | Seq.Cons (hd,tl) -> Some (hd,tl)
  | Seq.Nil -> None

type 'a nonEmptySq =
  NonEmptySq of 'a * 'a Seq.t

let nonEmptyOfSq sq =
  uncons sq |> Option.map (fun (hd,tl) -> NonEmptySq (hd,tl))
    

let nextNonEmpty (NonEmptySq (curr,tl)) =
  uncons tl |> Option.map (fun (hd,tl) -> (curr,NonEmptySq (hd,tl)))

let getCurrNonEmpty (NonEmptySq (curr,_)) =
  curr

type breakoutWalkState =
  { out : int * int
  ; limits : (int * Seq.t int,int * Seq.t int)
  ; dir : direction
  }

(*
type boundaryState =
  Lower
| Higher
| Between

let updateLimits (a,b) x =
  let (neMin,neMax) = sortArgsBy (fun (NonEmptySq (a,_) (NonEmptySq (b,_) -> b > a))) in
  let min,max = (getCurrNonEmpty neMin, getCurrNonEmpty neMax) in
  if x < min then
    (Lower,()
  else if x > max then
    (Higher,(a,nextNonEmpty 
  else
    Between*)


            



let breakoutWalk start steps minSq maxSq =
  let safeSteps = Seq.map positive steps in
  let tryInit =
    match (uncons minSq, uncons maxSq) with
    | (Some (b1, b1tl),Some (b2,b2tl)) ->
               Some { out = start ;
                 limits = ((b1,b1tl),(b2,b2tl));
                 dir = Up
               }
    | (_,_) -> None
  in
  let update (Posi step) { out; limits ; dir } =
    let (b1,b2) = limits in
    let ((min,minTl,(max,maxTl)) = sortArgsBy (fun ((x,_),(y,_)) -> x < y) b1 b2 in
    if out < min then
      { out = out + step;
        ; limits (
    
(*
let grow sq whenNil f =
  match sq () with
  | Seq.Nil -> whenNil
  | Seq.Cons(hd,tail) -> f hd tail

let walki2 start stepSq =
  let f (prev,steps) =
    grow steps None (fun step tl -> let n = prev + step in Some (n,(n,tl)))
  in*)
    

let manyWalks =
  (st 76) |> Seq.map oneWalk |> concat

let pitchSq =
  effect writer manyWalks

let notes =
  st makeNoteOfInts
  <*> pitchSq
  <*> (st 80)
  <*> (st (seci 1.0))
  <*> (st 1)

let midiFun _ =
  let map = Seq.map in
  notes
  |> syncOverClock (clockGen (seq [1.5;2.0]) )
  |> map ofOpt |> serialize |> map toRaw


let () = Midi.playMidi midiFun Process.sample_rate
