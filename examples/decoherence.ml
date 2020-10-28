open Cisp
open Seq


(*open Format *)
    
let msec = map sec 

let samplesize = 0.1

 let sumEight =
     [0;1;2;3;4;5;6;7]
     |> List.map (fun n -> Process.inputSeq n)
     |> (fun lst -> List.fold_right (+.~) lst (st 0.0) )
  
               
let mkDigi () =      
  let wl = rvf (st (-64.0)) (st 30.0) |> map mtof |> map (fun x -> 1.0 /. x) in
  let bottom = (st 0.0) in
  let top = tmd (lift rvf 1.0 9.0) (seq [0.0;sec 10.0]) in
  let buffer = Array.make (sec 10.0 |> Int.of_float) 0.0 in
  let writer = write buffer (countTill <| cap buffer) (sumEight  |> map tanh |> ( ( *.~ ) (st 1.3) )) in
  let myIndex = tline (tmd (rvf (st 0.5) (st 5.0)) wl) ([bottom;top] |> ofList |> transpose |> concat) in
  let mkOut ()= indexLin buffer myIndex in
  (effect writer (mkLots 15 mkOut),mkLots 15 mkOut)


let mkZigzag channelN =
  let buffer = Array.make (sec 5.0 |> Int.of_float) 0.0 in
  let input = Process.inputSeq channelN in
  let writer = write buffer (countTill (cap buffer)) input in
  let tees () = fractRandTimer (ch [|0.01;0.003;0.0001;0.1;0.2;0.5;1.0;2.0|]) in
  let transpose () = tmd (rvf (st 0.5) (st 5.0)) (ch [|0.5;1.5;0.75;1.25;2.0|]) in
  let speed = tmd (tees ()) (seq [-1.0;1.1] *.~ transpose ()) in                       
  let idx = walk 0.0 speed |> map (clip 0.0 (sec 4.9)) in
  let randEnv = tline (ch [|0.5;1.0;3.0;5.0|]) (seq [0.1;1.0;1.0;0.1]) in
  let reader () = indexCub buffer idx *.~ randEnv in
  effect writer (reader ())
  
let mkMirror channelN = 
  let buffer = Array.make (sec 20.0 |> Int.of_float) 0.0 in
  let input = Process.inputSeq channelN in
  let writer = write buffer (countTill (cap buffer)) input in
  let speed = ([|1.0;1.1;1.2;1.3;1.4;1.5;1.6;1.7;1.8|]).(channelN) in
  let idx = tline (speed *. 20.0 |> st) (seq [0.0;sec 20.0]) in
  let reader = indexCub buffer idx in
  effect writer reader     
               
let mkStutter channelN =
  let pitch = tmd (st samplesize) (ch [|1.0;0.9;1.1;1.25;0.8;0.2;3.0|] |> hold (seq [2;3;2;1;4])) in
  let baseline = walk 0.0 pitch |> map (wrapf 0.0 (sec samplesize)) in
  let entrydelay = (st samplesize) in
  let offset = (rv (st 0) (st 50) |> hold (seq [3;2;2;3;5]) |> floatify) *.~ (st <| sec samplesize) in
  let randomSteppy = tmd entrydelay offset in
  let jumpyLine = baseline +.~ randomSteppy in
  
  let buffer = Array.make (sec 5.0 |> Int.of_float) 0.0 in 
  let input = Process.inputSeq channelN in
  let hpf = bhpf_static 100.0  0.9 input in
  let speedyMod = tline (rvf (st 0.5) (st 10.0)) (ch [|0.5;1.0;20.0;5.0;100.0|] |> hold (st 2)) in
  let percEnv () =
   decayPulse 0.999 (pulsegen speedyMod) in
  let myReader () = (indexCub buffer (jumpyLine)) *.~ percEnv () in
  
  let writer = write buffer (countTill (cap buffer)) hpf in
  let joined = effect writer (myReader ()) in
  joined


let mkBoerman nInput =
  let place =
    (seq [0.0;4.0 |> sec]) 
  in
  let dura =
   (ch [|1.0;2.0;3.0;4.0;5.0;6.0;7.0;8.0|])
  in       
  let myLineTest =
    tline dura place 
  in
  let buffer = Array.make (seci 5.0) 0.0 in 
  let input = Process.inputSeq nInput in
  let writer = write buffer (countTill <| cap buffer) input in
  let myReader = indexCub buffer myLineTest in
  let joined = effect writer myReader in
  joined

let mkBoerman2 nInput =
  let place =
    (seq [0.0;4.0 |> sec]) 
  in
  let dura =
   (ch [|1.0;2.0;3.0;2.99;3.01;4.0;8.0;16.0;3.99;4.01;4.02;3.89|])
  in       
60.0 boer
let l11,r11 = mks 460.0 1.0 stupid
          

(*
let scoreL = playScore (mkScore [l1])   
let scoreR = playScore (mkScore [r1])*)

let scoreL = playScore (mkScore [l1;l2;l3;l4;l5;l6;l7;l8;l9;l10;l11])
let scoreR = playScore (mkScore [r1;r2;r3;r4;r5;r6;r7;r8;r9;r10;l11])

let () = 
  Jack.playSeqs 8 Process.sample_rate [effect (masterClock) scoreL;scoreR]
