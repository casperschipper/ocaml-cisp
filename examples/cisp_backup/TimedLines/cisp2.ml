open Cisp
open Seq
   
let sec s = !Process.sample_rate *. s

let msec = map sec 

let maskBound () = line (seq [0.0;sec 10.0]) (ch [|17.;13.5|])

(* let skip n =*)
  

(* this takes three types of readers and combines them into one index *)

let slowNoise speed = 
  let arr = rvf (st 0.0) (st 1.0) |> take (1024 * 512) |> Array.of_seq in
  let index = walk 0.0 speed in
  indexCub arr index
    
    
(* let read1 () = line (seq [0.0; sec 10.0]) (seq [3.0;5.0;7.0;14.0] |> msec)
let read2 () = line (seq [0.0; 10.0]) (ch [|10.0;9.99;5.0;15.0;1.02;10.1|] |> msec)
let read3 () = line ([maskBound ();maskBound ()] |> ofList |> transcat) (st (sec 2.0)) *)

(* weave arrays, timed is used as an index into the three different types.
   note that this is kind of similar to transCat, since they are concatinated in order *)
let speed =
  ch ([|10.0;20.0;30.0;1.0;5.0|]) |> tmd (st 1.0) |> map (fun x -> 1.0 /. (sec x))
  
let slow =
  map (( *. ) (sec 10.0)) (slowNoise speed)

let offset =
  (tline (st 30.0)  (seq [0.0;sec 10.0]))

let loopSize =
  (1.0 |> sec |> st)
  

let mylines =
  tline (st 0.01)  ([|offset;offset +.~ loopSize|] |> fun arr ->  weaveArray arr (seq [0;1]) )

let combi =
  mylines
  
           
              
let () =
  let buffer = Array.make (sec 10.0 |> Int.of_float) 0.0 in
  let input = Process.inputSeq 0  in
  let writer = write buffer (countTill <| cap buffer) input in

  let mkOut () = indexLin buffer combi in
  let joined = syncEffect (mkOut ()) writer in
  Jack.playSeqs 1 Process.sample_rate [ joined ; mkOut ()]

     

    
    
 
    
    
