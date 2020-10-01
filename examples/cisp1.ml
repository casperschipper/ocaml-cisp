open Cisp
open Seq

let sr = ref 44100.0

let sec s = !sr *. s

(* (float -> float) ->  seq.t float -> seq.t float *)

(* frequency is the thing *)

let phase_inc = (1.0 /. !sr) *. Float.pi *. 2.0

(** recursive
 @control : the control stream, that allows us to customize the update
 @init : the initial state (can be anything!)
 @update : takes a state and one value of control then produces a new state
 @evaluate : takes current state and produces the next output value **)


                   
(* 
pattern:
main : controlSq, state  4
deconstruct control signal in x :: xs
f : state -> value
g : x -> state -> state
in
cons (f state, self xs (
 *)


  
  
   
   
let () =
  let times = grow 0.01 2.0 26 |> Array.of_list in
  (*let audioIn = Process.inputSeq 0 in*)
  let ratio = (line (ch [|0.001; 0.1;0.3;1.0;2.0;4.0;80.0|]) (map sec <| ch times)) in
  let depth = (line (seq [0.001; 0.2; 1.0;2.0;4.0;80.0]) (map sec <| ch times)) in
  let sinewave f = fm_osc (st f) ratio depth in
  let rec lots n =
    if n != 0 then
      (st 0.1 *.~ sinewave (Random.float (40.0) |> mtof)) +.~ (lots (n-1))
    else
      (st 0.0)
  in
    
  
  (*let proc =
    zipWith (fun x y -> x *. y) sinewave audioIn |> map (fun x -> x *. 1.) |> Process.ofSeq
  in *)
  Jack.play 0 Process.sample_rate [lots 12 |> Process.ofSeq ; lots 12 |> Process.ofSeq] 
