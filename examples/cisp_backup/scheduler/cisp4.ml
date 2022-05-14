open Cisp
open Seq
(* open Format   *)
   
let sec s = !Process.sample_rate *. s

let msec = map sec 

let myLineTest () =
  tline (ch [|0.1;0.5;1.0;2.0;1.5;0.75|]) (seq [0.0;5.0 |> sec])

let sum lst =
  List.fold_right (fun x acc -> x +.~ acc) lst (st 0.0)

let singleton a =
  [a]
              
let () =
  let buffer = Array.make (sec 5.0 |> Int.of_float) 0.0 in 
  let input = Process.inputSeq 0 in
  let input_2 = Process.inputSeq 1 *.~ (st 0.1) |> map (clip (-1.0) 1.0) in
  let input_osc = input +.~ input_2 +.~ (osc (st 440.0) *.~ (st 0.1)) in
  let writer = write buffer (countTill <| cap buffer) input_osc in
  let myReader () = indexLin buffer (myLineTest ()) in
  let timefied = effectSync masterClock (myReader ()) in
  let joined = effectSync writer timefied in
  Jack.playSeqs 1 Process.sample_rate [joined +.~ mkLots 4 myReader;mkLots 5 myReader]

