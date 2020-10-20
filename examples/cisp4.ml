open Cisp
open Seq
   
let sec s = !Process.sample_rate *. s

let msec = map sec 

let myLineTest () =
  tline (st 0.5) ([st 0.0;rvf (st 0.0) (st 1.0) |> map sec] |> ofList |> transpose |> concat)

let sum lst =
  List.fold_right (fun x acc -> x +.~ acc) lst (st 0.0)

let singleton a =
  [a]
              
let () =
  let buffer = Array.make (sec 1.0 |> Int.of_float) 0.0 in 
  let input = Process.inputSeq 0 in
  let writer = write buffer (countTill <| cap buffer) input in
  let myReader () = indexLin buffer (myLineTest ()) in
  let timefied = effectSync masterClock (myReader ()) in
  let joined = effectSync writer timefied in
  
  Jack.playSeqs 1 Process.sample_rate [joined +.~ mkLots 4 myReader;mkLots 5 myReader]
    
        
    
