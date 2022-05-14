open Cisp
open Seq
   
let sec s = !Process.sample_rate *. s

          
let kees =
  ch [|0.99;1.01;2.0;3.0;4.0;5.0|] |> map (( *. ) 5.0) |> map sec

let theTimed =
  fractRandTimer kees
  
(*let noisy = rvf (st 0.0) <| line (seq [0.0;2.0;2.0;0.0]) (st <| sec 5.0)*)

let top = (line (seq [10.0; sec 10.0]) (ch [|13.0;17.0|]))

let target = (seq [0.0;10.0]) |> map sec

let myindex = (line target kees)  (* +.~ noisy*)
           
              
let () =
  let buffer = Array.make (sec 10.0 |> Int.of_float) 0.0 in
  let writer = write buffer (countTill <| cap buffer) (Process.inputSeq 0) in

  let mkOut () = indexLin buffer myindex in
  let joined = syncEffect (mkOut ()) writer in
  Jack.playSeqs 1 Process.sample_rate [ joined +.~ mkLots 15 mkOut ; mkLots 15 mkOut]

     

    
    
