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
  let bottom = tmd (lift rvf 1.0 7.0) (seq [0.0;sec 10.0]) in
  let top = tmd (lift rvf 1.0 9.0) (seq [0.0;sec 10.0]) in
  let buffer = Array.make (sec 10.0 |> Int.of_float) 0.0 in
  let writer = write buffer (countTill <| cap buffer) (sumEight  |> map tanh |> ( ( *.~ ) (st 1.3) )) in
  let myIndex = tline (tmd (rvf (st 0.5) (st 5.0)) wl) ([bottom;top] |> ofList |> transpose |> concat) in
  let mkOut ()= indexLin buffer myIndex in
  (effect writer (mkLots 5 mkOut),mkLots 5 mkOut)


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
  let speed = ([|0.8;0.9;1.0;1.1;1.2;1.15;1.05;1.25|]).(channelN) in
  let idx = tline (speed *. 5.0 |> st) (seq [0.0;sec 5.0]) in
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
  let detune =
    tline (lift rvf 10.0 20.0) (rvf (st (-0.03))  (st 0.03))
  in
  let dura =
   (ch [|3.0;3.5;4.0;4.5;5.0|]) +.~ detune
  in       
  let myLineTest =
    tline dura place 
  in
  let buffer = Array.make (seci 5.0) 0.0 in 
  let input = Process.inputSeq nInput |> map tanh in
  let writer = write buffer (countTill <| cap buffer) input in
  let myReader () = indexCub buffer myLineTest in
  let joined = effect writer (myReader ()) in
  joined
 
let mkSlowNoiseBuff nInput =
  let freq = tmd (rvf (st 1.0) (st 10.0)) (ch ([|2.0;4.0;5.0;6.0;1.0;0.5|])) |> map sec in
  let index () = slowNoise ((st 1.0) /.~ freq) |> map (fun x -> x *. (sec 5.0)) in
  let buffer = Array.make (sec 5.0 |> Int.of_float) 0.0 in
  let input = Process.inputSeq nInput in
  let writer = write buffer (countTill <| cap buffer) input in
  let myReader () = indexCub buffer (index ()) in
  let joined = effect writer (myReader ()) in
  joined
  
let softclip signal = map tanh signal
  
let voiceWithInputs voicef inputs =
  List.fold_left (fun acc inpt -> (voicef inpt) +.~ acc) (st 0.0) inputs |> softclip

let makeStereo mkfun =
  ( voiceWithInputs mkfun (rangei 0 3 |> toList)
  , voiceWithInputs mkfun (rangei 4 7 |> toList) )
  

let boer2 = makeStereo mkBoerman2
let boer = makeStereo mkBoerman 
let noiseL,noiseR = makeStereo mkSlowNoiseBuff 
let stutterL,stutterR = makeStereo mkStutter 
let mirrorL, mirrorR = makeStereo mkMirror 
let zigzag = makeStereo mkZigzag 

let mks a b (fl,fr) = (mkSection (seci a) (seci b) fl),(mkSection (seci a) (seci b) fr)
        
let l1,r1 = mks 0.0 120.0 boer2
let l2,r2 = mks 30.0 5.0 (mkDigi ())
let l3,r3 = mks 35.0 3.0 (stutterL,stutterR)
let l4,r4 = mks 120.0 20.0 (mirrorL,mirrorR)
let l5,r5 = mks 115.0 5.0 (noiseL,noiseR)
let l6,r6 = mks 120.0 60.0 (mkDigi ())
let l7,r7 = mks 200.0 30.0 (noiseL,noiseR)
let l8,r8 = mks 230.0 120.0 boer
let l9,r9 = mks 345.0 20.0 zigzag
          

        


let scoreL = playScore (mkScore [l1;l2;l3;l4;l5;l6;l7;l8;l9])
let scoreR = playScore (mkScore [r1;r2;r3;r4;r5;r6;l7;l8;l9])

let () = 
  Jack.playSeqs 8 Process.sample_rate [effect (masterClock) scoreL;scoreR]
