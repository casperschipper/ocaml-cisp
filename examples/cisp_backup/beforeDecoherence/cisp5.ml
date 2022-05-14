open Cisp
open Seq
   
let sec s = !Process.sample_rate *. s

let msec = map sec 

let myLineTest =
  tline (st 0.01) (seq [(-1.0);1.0])


(* lets try a simple line *)
let () =
  Jack.playSeqs 1 Process.sample_rate ([myLineTest] |> inTime)

     

    
    

    
    
    
        
    
