open Cisp
open Seq

   
let sec s = !Process.sample_rate *. s

let sumEight =
  [0;1;2;3;4;5;6;7]
  |> List.map (fun n -> Process.inputSeq n)
  |> (fun lst -> List.fold_right (+.~) lst (st 0.0) )
  
  
                  
let () =
  let bottom = tline (st 33.0) (seq [0.0;sec 10.0]) in
  let top = tline (st 99.0) (seq [0.0;sec 10.0]) in
  let buffer = Array.make (sec 10.0 |> Int.of_float) 0.0 in
  let writer = write buffer (countTill <| cap buffer) (sumEight |> map tanh |> ( ( *.~ ) (st 0.8) )) in
  let myIndex = tline (tmd (st 4.0) (ch [|0.5;0.1;4.0;8.0;0.01;0.001|])) ([bottom;top] |> ofList |> transpose |> concat) in
  let mkOut () = indexLin buffer myIndex in
  let joined = effect writer (mkOut ()) in
  Jack.playSeqs 8 Process.sample_rate [ effect masterClock joined ; mkOut () ]

     

    
    
