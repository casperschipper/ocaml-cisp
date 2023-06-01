open Cisp


(*
idea: write a graph based synthesis 
*)

type oscillator =
  { phase : float 
  ; freq : float 
  }

let upd (osc : oscillator) =
  { osc with phase = osc.phase +. (osc.freq /. !Process.sample_rate ) }

let listen osc = 
  sin (osc.phase *. 2.0 *. 3.14159)



type state = int

let init = 0

let listen state =
  match state with
  | 0 -> -1.0
  | 1 -> 0.0
  | 2 -> 1.0
  | 3 -> 0.5
  | _ -> 0.0
  
let next state =


let main () =   
  Jack.playSeqs 0 Process.sample_rate []

