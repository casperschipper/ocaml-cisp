open Cisp
open Seq
   
let sec s = !Process.sample_rate *. s

let msec = map sec 

let maskBound () = line (seq [0.0;sec 10.0]) (ch [|17.;13.|])

(* this takes three types of readers and combines them into one index *)
                 
let read1 () = line (seq [0.0; sec 10.0]) (seq [3.0;5.0;7.0;14.0] |> msec)
let read2 () = line (seq [0.0; 10.0]) (lift rvf 0.0 15.0 |> msec)
let read3 () = line ([maskBound ();maskBound ()] |> ofList |> transcat) (st (sec 1.0))

(* weave arrays, timed is used as an index into the three different types.
   note that this is kind of similar to transCat, since they are concatinated in order *)
let combi = weaveArray [|read1 ();read2 ();read3 ()|] (timed (ch ([|0.1;0.5;0.2|])) (countTill 3))
           
              
let () =
  let buffer = Array.make (sec 10.0 |> Int.of_float) 0.0 in
  let writer = write buffer (countTill <| cap buffer) (Process.inputSeq 0) in

  let mkOut () = indexLin buffer combi in
  let joined = syncEffect (mkOut ()) writer in
  Jack.playSeqs 1 Process.sample_rate [ joined +.~ mkLots 10 mkOut ; mkLots 10 mkOut]

     

    
    
 
