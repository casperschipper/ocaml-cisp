open Cisp
open Seq

   
let sec s = !Process.sample_rate *. s
          
let sumEight =
  [0;1;2;3;4;5;6;7]
  |> List.map (fun n -> Process.inputSeq n)
  |> (fun lst -> List.fold_right (+.~) lst (st 0.0) )
  
(* TODO REMOVE MASTERCLOCK IF COPIED INTO MAIN *)
                  
let () =
  let wl = rvf (st (-80.0)) (st 0.0) |> map mtof |> map (fun x -> 1.0 /. x) in
  let bottom = tmd (lift rvf 0.0 3.0) (seq [0.0;sec 10.0]) in
  let top = tmd (lift rvf 0.0 3.0) (seq [0.0;sec 10.0]) in
  let buffer = Array.make (sec 10.0 |> Int.of_float) 0.0 in
  let writer = write buffer (countTill <| cap buffer) (sumEight |> map tanh |> ( ( *.~ ) (st 0.8) ) |> bhpf_static 50.0 0.9)  in
  let myIndex = tline (tmd (rvf (st 0.5) (st 5.0)) wl) ([bottom;top] |> ofList |> transpose |> concat) in
  let mkOut () = indexLin buffer myIndex in
  let joined = effect writer (mkOut ()) in
  Jack.playSeqs 8 Process.sample_rate [ effect masterClock joined ; mkOut ()] 

     

    
    
