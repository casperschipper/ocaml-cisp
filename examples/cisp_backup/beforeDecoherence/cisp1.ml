open Cisp
open Seq
(* open Format   *)
   
let sec s = !Process.sample_rate *. s

let msec = map sec 

let place =
  (seq [0.0;4.0 |> sec]) +.~ (rvf (st 0.0) (st 3.0))

let dura =
   (ch [|4.0;2.0;3.0;5.0|]) |> loop (st 2) (st 2)
         
let myLineTest () =
  tline dura place +.~ (rvf (st 0.0) (st 2.0))

let sum lst =
  List.fold_right (fun x acc -> x +.~ acc) lst (st 0.0)

let feedbackAmp () =
  tline (ch [|4.0;5.0;7.0;3.33333|]) (seq [0.0;1.0])

let rec sumInputs startN endN =
  if startN > endN then
    st (0.0)
  else
    begin
    let gain = 0.8 /. Float.of_int (endN - startN) in
    let attenuate signal = map (fun x -> x *. gain) signal in
    if startN = endN then 
      Process.inputSeq startN |> attenuate
    else
      ((Process.inputSeq startN |> attenuate) +.~ (sumInputs (startN+1) endN))
    end
  
              
let () =
  let buffer = Array.make (sec 5.0 |> Int.of_float) 0.0 in 
  let input = Process.inputSeq 0 in
  (* let input_2 = Process.inputSeq 1 *.~ (st 0.5) |> map (clip (-1.0) 1.0) in 
  let input_osc = input +.~ (input_2 *.~ feedbackAmp ()) in
  let hpf = bhpf_static 100.0 0.9 input_osc in *)
  let writer = write buffer (countTill <| cap buffer) input in
  let myReader () = indexCub buffer (myLineTest ()) in
  let timefied = effectSync masterClock (myReader ()) in
  let joined = effectSync writer timefied in
  Jack.playSeqs 8 Process.sample_rate [joined +.~ mkLots 4 myReader;mkLots 5 myReader]

