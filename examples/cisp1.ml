open Cisp
open Seq
(*open Format *)
   
let msec = map sec 

let samplesize = 0.1

let computeTimeConst x = 1.0 -. exp ((-2.2) /. x)

type avgState =
  { attack : float
  ; release : float
  ; filterC : float
  ; idx : int
  ; currentAvg : float
  ; out : float }
               
let avg windowSize sq =
  let init () =
    { attack : 0.01
    ; release : 0.01
    ; size = windowSize
    ; currentAvg = 0.0
    ; out = 0.0 }
  in
  recursive
    sq
    (init ())
    (fun c state ->
    )
    
let () = 
  Jack.playSeqs 8 Process.sample_rate 
    

