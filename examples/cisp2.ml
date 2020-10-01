open Cisp
   

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
  let freq = timed (seq [440.0;220.0;110.0]) (st 0.1) in
  let out = osc freq in
  Jack.play 0 Process.sample_rate [ out |> Process.ofSeq ]

    

    
