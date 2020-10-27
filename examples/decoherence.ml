open Cisp
open Seq


(*open Format *)
    
let msec = map sec 

let samplesize = 0.1

let voiceWithInputs voicef inputs =
  List.fold_left (fun acc inpt -> (voicef inpt) +.~ acc) (st 0.0) inputs

let mkZigzag channelN =
  let buffer = Array.make (sec 5.0 |> Int.of_float) 0.0 in
  let input = Process.inputSeq channelN in
  let writer = write buffer (countTill (cap buffer)) input in
  let tees () = fractRandTimer (ch [|0.01;0.003;0.0001;0.1;0.2;0.5;1.0;2.0|]) in
  let transpose () = tmd (rvf (st 0.5) (st 5.0)) (ch [|0.5;1.5;0.75;1.25;2.0|]) in
  let speed = tmd (tees ()) (seq [-1.0;1.0] *.~ transpose ()) in                       
  let idx = walk 0.0 speed |> map (clip 0.0 (sec 4.9)) in
  let randEnv = tline (ch [|0.5;1.0;3.0;5.0|]) (seq [0.1;1.0;1.0;0.1]) in
  let reader = indexCub buffer idx *.~ randEnv in
  effect writer reader  
  
let mkMirror channelN = 
  let buffer = Array.make (sec 20.0 |> Int.of_float) 0.0 in
  let input = Process.inputSeq channelN in
  let writer = write buffer (countTill (cap buffer)) input in
  let speed = rvfi 0.9 1.1 in
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
  let myLineTest () =
    tline dura place 
  in
  let buffer = Array.make (sec 5.0 |> Int.of_float) 0.0 in 
  let input = Process.inputSeq nInput in
  let hpf = bhpf_static 20.0 0.9 input *.~ (st 2.0) |> map (clip (-1.0) 1.0) in
  let writer = write buffer (countTill <| cap buffer) hpf in
  let myReader () = indexCub buffer (myLineTest ()) in
  
  let joined = effectSync writer (myReader ()) in
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
  


  
let boerL = voiceWithInputs mkBoerman (rangei 0 3 |> toList)
let boerR = voiceWithInputs mkBoerman (rangei 4 7 |> toList)
let noiseL = voiceWithInputs mkSlowNoiseBuff (rangei 0 3 |> toList)
let noiseR = voiceWithInputs mkSlowNoiseBuff (rangei 4 7 |> toList)
let stutterL = voiceWithInputs mkStutter (rangei 0 3 |> toList)
let stutterR = voiceWithInputs mkStutter (rangei 4 7 |> toList)
let mirrorL = voiceWithInputs mkMirror (rangei 0 3 |> toList)
let mirrorR = voiceWithInputs mkMirror (rangei 4 7 |> toList)
let zigzagL = voiceWithInputs mkZigzag (rangei 0 3 |> toList)
let zigzagR = voiceWithInputs mkZigzag (rangei 4 7 |> toList)
            

let l1 = mkSection 0 (120.0 |> seci) zigzagL
let r1 = mkSection 0 (120.0 |> seci) zigzagR
 


let scoreL = playScore (mkScore [l1])
let scoreR = playScore (mkScore [r1])

let () = 
  Jack.playSeqs 8 Process.sample_rate [effect (masterClock) scoreL;scoreR]
    

