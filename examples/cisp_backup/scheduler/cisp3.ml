open Cisp
open Seq
   
let sec s = !Process.sample_rate *. s

let msec = map sec 

let myLineTest =
  tline (st 5.0) (seq [0.0;sec 5.0])

let () =
  let buffer = Array.make (sec 5.0 |> Int.of_float) 0.0 in
  let input = Process.inputSeq 0 in
  let writer = write buffer (countTill <| cap buffer) input in
  let myReader () = indexLin buffer (myLineTest)  in
  let joined = syncEffect (myReader ()) writer in
  Jack.playSeqs 1 Process.sample_rate (inTime [joined;myReader ()])

     

    
    
 
    
    
        
    
