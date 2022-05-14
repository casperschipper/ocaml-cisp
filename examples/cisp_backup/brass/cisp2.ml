open Cisp
open Midi

let ofOpt optEvt = match optEvt with Some evt -> evt | None -> SilenceEvent


type direction =
  Up
| Down


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

type limits = Limits of (int * (int Infseq.t)) * (int * (int Infseq.t))

let clipLim (Limits ((b1,_),(b2,_))) x =
  let (min,max) = sortArgs b1 b2 in
  if x < min then
    min
  else
    if x > max then
      max
    else
      max
  
type breakoutWalkState =
  { out : int
  ; limits : limits
  ; dir : direction
  }



let breakoutWalk start steps minInfSq maxInfSq =
  let bounceLimits limits curDir x  =
    let Limits (b1,b2) = limits in 
    let ((min,minTl),(max,maxTl)) = sortArgsBy (fun a b -> fst a < fst b) b1 b2 in
    if x <= min then
      (Limits (Infseq.uncons minTl,(max,maxTl)),Up)
    else
      if x >= max then
        (Limits ((min,minTl),Infseq.uncons maxTl),Down)
      else
        (Limits (b1,b2), curDir)
  in
  let safeSteps = Seq.map positive steps in 
  let init =
    { out = start
    ; limits = (mapBoth Infseq.uncons (minInfSq,maxInfSq)) |> (fun (l1,l2) -> Limits (l1,l2))
    ; dir = Up 
    }
  in
  let update (Posi step) { out; limits ; dir } =
    let (newLimits,newDir) = bounceLimits limits dir out in
    let operator = match newDir with Up -> ( + ) | Down -> ( - ) in
    ({ out = (operator out step); limits = newLimits ; dir = newDir })
  in
  let eval state =
    state.out
  in
  recursive safeSteps init update eval


type wanderDir =
  Dec
| Inc
| Static

type wandererState =
  { out : int
  ; vector : int * wanderDir
  ; targets : int Infseq.t
  ; target : int
  }


let wanderer start (targets: int Infseq.t) =
  let chooseDirection current target =
    if current = target then
      Static
    else
      if current > target then
        Dec
      else
        Inc
      
  in
  let init =
    { vector = (1,Static)
    ; out = start
    ; targets = targets
    ; target = start
    }
  in
  let move (step,dir) prev =
    match dir with Inc -> prev + step | Dec -> prev - step | Static -> prev
  in
  let f state =
    if state.out = state.target then
      let (newTarget, tail) = Infseq.uncons state.targets in
      let newDir = (1,chooseDirection state.out newTarget) in
      (state.out,
      { out = move newDir state.out
      ; targets = tail
      ; vector = newDir
      ; target = newTarget
      })
    else
      (state.out,{ state with out = move state.vector state.out })
  in
  Infseq.unfold f init 
  

type dwalkState = {
    out : int
   ;dir : direction
  }

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
  recursive ctrlSq init update eval

let intervArr = [|7;4;4;7;3|] 

let writer =
  let index = wrappedCount intervArr in
  let generator = ch [|7;4;3;5|] in
  let wrf (i, v) = writeOne intervArr i v in
  zip index generator |> Seq.map wrf |> syncEffectClock (interval (lift rv 1 10))

let oneWalk start =
  let idx = wrappedCount intervArr in
  let steps = (index intervArr idx |> take 5) *~ (st (-1)) in
  walki start steps

(*type walkmind direction is state *)
(* a walk that updates boundaries only when hit *)


let manyWalks =
  (st 76) |> Seq.map oneWalk |> concat

let pitchSq =
  effect writer manyWalks


let simpleWalk = breakoutWalk
                   3
                   (seq [1])
                   (Infseq.repeat 0)
                   (seq [0;1;2;3;4;5;6;7;8;9;8;7;6;5;4;3;2;1] |> Infseq.cycleSq)

let camino = wanderer 0 (Infseq.cycleSq (seq [0;10;0])) 
    

let simplePitch =
  index [|0;4;7;12;16;19;24;26;28;30;32|] simpleWalk +~ (st 52) 

let () =  Seq.iter (fun x -> print_int x; print_string "\n") (camino |> Infseq.take 30) ; flush stdout 
(*

let notes =
  st makeNoteOfInts
  <*> simplePitch
  <*> (st 80)
  <*> (st (seci 0.1))
  <*> (st 1)

let midiFun _ =
  let map = Seq.map in
  notes
  |> syncOverClock (clockGen (seq [12.0]) )
  |> map ofOpt |> serialize |> map toRaw


let () = Midi.playMidi midiFun Process.sample_rate *)
