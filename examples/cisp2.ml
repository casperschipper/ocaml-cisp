open Cisp
open Seq
(* open Format   *)
   
let sec s = !Process.sample_rate *. s

let msec = map sec 

let samplesize = 0.25

let pitch = tmd (st samplesize) (ch [|0.5;1.0;1.25;0.75;0.99;1.1|] |> hold (seq [2;3;2;1;4]))   
let baseline = walk 0.0 pitch |> map (wrapf 0.0 (sec samplesize))
let entrydelay = (st samplesize)
let offset = (rv (st 0) (st 50) |> hold (seq [3;2;2;3;5]) |> floatify) *.~ (st <| sec samplesize) 
let randomSteppy = tmd entrydelay offset
let jumpyLine = baseline +.~ randomSteppy

let decay fb inSq =
  let rec aux prev inSq () = 
  match inSq () with
  | Cons(hd, tail) -> Cons (hd +. prev, aux (hd *. fb) tail)
  | Nil -> Nil
  in
  aux 0.0 inSq 

let env =
  let n = sec 0.1 |> Int.of_float |> ((-) 1) in
  let trigg = pulse (st n) (st 1.0) (st 0.0) in
  decay 0.5 trigg


let singleton a =
  [a]
              
let () =
  let buffer = Array.make (sec 5.0 |> Int.of_float) 0.0 in 
  let input = Process.inputSeq 0 in
  let input2 = Process.inputSeq 1 in
  let hpf = input2 |> bhpf_static 100.0 0.9 in
  let myReader () = indexCub buffer (jumpyLine) *.~ env in
  let timefied = effect masterClock (myReader ()) in
  let writer = write buffer (countTill (cap buffer)) (input +.~ ( hpf *.~ st 0.1 |> map (clip (-0.1) (1.0)))) in
  let joined = effect writer timefied in
  Jack.playSeqs 3 Process.sample_rate [joined +.~ mkLots 6 myReader |> map (clip (-1.0) 1.0);mkLots 6 myReader |> map (clip (-1.0) 1.0)]

