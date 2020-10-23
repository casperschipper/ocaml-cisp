open Cisp
open Seq
   
let sec s = !Process.sample_rate *. s

(* (float -> float) ->  seq.t float -> seq.t float *)

(* frequency is the thing *)

let phase_inc = (1.0 /. !Process.sample_rate) *. Float.pi *. 2.0

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



let timerSeq =
  ch [|0.1;0.2;0.4|] |> map sec

let fractalTime =
  fractRandTimerN 3 timerSeq
  
(* let noisy = rvf (st 0.0) <| line (seq [0.0;20.0;20.0;0.0]) (st <| sec 5.0) |> loop (st 24) (st 48) *)

let myindex = (line (seq [0.0;sec 5.0]) (fractalTime)) (*  +.~ noisy*)
 
              
let () =
  let buffer = Array.make (sec 5.0 |> Int.of_float) 0.0 in
  let writer = write buffer (countTill <| cap buffer) (Process.inputSeq 0) in

  let mkOut () =indexLin buffer myindex in
  let joined = syncEffect (mkOut ()) writer in
  Jack.play 1 Process.sample_rate [ joined +.~ mkLots 4 mkOut |> Process.ofSeq ; mkLots 5 mkOut |> Process.ofSeq ]

     

    
    
