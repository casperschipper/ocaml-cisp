open Cisp
open Seq


(*open Format *)
    
let msec = map sec 

let samplesize = 0.1

let withEnv attack duration sq =
  let susT = duration -. (2.0 *. attack) in
  let env =  tline (seq [attack;susT;attack]) (seq [0.0;1.0;1.0;0.0]) in
  sq *.~ env
  

let sumEight =
  [0;1;2;3;4;5;6;7]
  |> List.map (fun n -> Process.inputSeq n)
  |> (fun lst -> List.fold_right (+.~) lst (st 0.0) )
  |> map (( *. ) 0.17)

let mkLoops channelN =
  let buffer = Array.make (sec 15.0 |> Int.of_float) 0.0 in
  let writer = write buffer (countTill <| cap buffer) (Process.inputSeq channelN |> map (( *. ) 0.2)) in
  let loopSize = rvfi 1.0 5.0 in
  let loopIndex = countTill (sec loopSize |> Int.of_float) |> floatify in
  let offset () = (st <| sec 0.5) *.~ (tmd (st 10.0) (rv (st 1) (st 100)) |> floatify) in
  let reader () = indexLin buffer (offset () +.~ loopIndex) in
  (effect writer <| mkLots 2 reader)
                             
  
let mkBrown cutoff =
   let buffer = Array.make (sec 10.0 |> Int.of_float) 0.0 in
   let writer = write buffer (countTill <| cap buffer) (sumEight  |> map tanh) in
   let top = tmd (st <| sec 3.0) (rv (st 1) (st 9000))  in
   let step =  (ch [|(-1.0);1.0|]) |> zipWith repeat top |> concat  in
   let index = walk 0.0 step  in
   let reader () = indexLin buffer index |> bhpf_static cutoff 0.9 in
   (effect writer <| reader (), reader ())
                            
   
               
let mkDigi () =      
  let wl = rvf (st (-64.0)) (st 30.0) |> map mtof |> map (fun x -> 1.0 /. x) in
  let bottom = (st 0.0) in
  let top = tmd (lift rvf 1.0 9.0) (seq [0.0;sec 10.0]) in
  let buffer = Array.make (sec 10.0 |> Int.of_float) 0.0 in
  let writer = write buffer (countTill <| cap buffer) (sumEight  |> map tanh |> ( ( *.~ ) (st 4.0) )) in
  let myIndex = tline (tmd (rvf (st 0.5) (st 5.0)) wl) ([bottom;top] |> ofList |> transpose |> concat) in
  let mkOut ()= indexLin buffer myIndex in
  (effect writer (mkLots 2 mkOut),mkLots 2 mkOut)


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
  let input = Process.inputSeq nInput |> bhpf_static 100.0 0.9 in
  let writer = write buffer (countTill <| cap buffer) input in
  let myReader = indexCub buffer myLineTest in
  let joined = effect writer myReader in
  joined
  
let mkBoermanFading nInput =
  let tabIndex =
    (seq [0.0;1.0]) 
  in
  let dura =
   (ch [|2.0;2.666666666666667;6.0;8.0|])
  in       
  let myLineTest =
    tline dura tabIndex
  in
  let bufIndex = myLineTest |> map (( *. ) <| sec 4.0) in
  let taper fadeTime x =
    match x with
    1.0 -> 1.0
    | att when x < fadeTime -> att /. 0.05
    | rel when x > (1.0 -. fadeTime) -> 1.0 -. ((rel -. (1.0 -. fadeTime)) /. fadeTime)
    | _ -> 1.0
  in
  let buffer = Array.make (seci 5.0) 0.0 in 
  let input = Process.inputSeq nInput |> bhpf_static 100.0 0.9 in
  let writerIdx = (countTill <| cap buffer) in
  let env = writerIdx |> map Float.of_int |> map (fun x -> x /. (cap buffer |> Float.of_int) |> taper 0.05) in
  let mupInput = input *.~ env in
  let writer =  write buffer writerIdx mupInput in
  let myReader = (indexCub buffer bufIndex) *.~ (myLineTest |> map (taper 0.05 )) in
  let joined = effect writer myReader in
  joined



let mkBoerman2 nInput =
  let place =
    (seq [0.0;4.0 |> sec]) 
  in
  let dura =
   (ch [|1.0;2.0;3.0;2.99;3.01;4.0;8.0;12.0;11.98;12.01;12.03;12.03;16.0;3.99;4.01;4.02;3.89|])
  in       
  let myLineTest =
    tline dura place 
  in
  let buffer = Array.make (seci 5.0) 0.0 in 
  let input = Process.inputSeq nInput |> map tanh |> bhpf_static 100.0 0.9  in
  let writer = write buffer (countTill <| cap buffer) input in
  let myReader () = indexCub buffer myLineTest in
  let joined = effect writer (myReader ()) in
  joined

let mkBoerman3 nInput =
  let place =
    (seq [0.0;4.0 |> sec]) 
  in
  let dura =
   (ch [|4.0;3.9;4.1;4.5;3.0;4.25;0.39|]) |> hold (st 4)
  in       
  let myLineTest =
    tline dura place 
  in
  let buffer = Array.make (seci 5.0) 0.0 in 
  let input = Process.inputSeq nInput |> map (( *. ) 0.2) |> map tanh in
  let writer = write buffer (countTill <| cap buffer) input in
  let myReader () = indexCub buffer myLineTest in
  let joined = effect writer (myReader ()) in
  joined
 
let mkSlowNoiseBuff nInput =
  let freq = tmd (rvf (st 1.0) (st 10.0)) (ch ([|2.0;4.0;5.0;6.0;1.0;0.5|])) |> map sec in
  let index () = slowNoise ((st 1.0) /.~ freq) |> map (fun x -> x *. (sec 2.0)) in
  let buffer = Array.make (sec 2.0 |> Int.of_float) 0.0 in
  let input = Process.inputSeq nInput |> map (( *. ) 0.2) in
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
  
let boer3 = makeStereo mkBoerman3
let boer2 = makeStereo mkBoerman2
let boer = makeStereo mkBoerman 
let noiseL,noiseR = makeStereo mkSlowNoiseBuff 
let stutter = makeStereo mkStutter 
let mirror= makeStereo mkMirror 
let zigzag = makeStereo mkZigzag 
let loopsy = makeStereo mkLoops
           
let mks a b (fl,fr) = (mkSection (seci a) (seci b) fl),(mkSection (seci a) (seci b) fr)



let ending = (osc (st 220.0) |> withEnv 3.0 30.0, osc (st 110.0) |> withEnv 3.0 30.0)

let hardClip sq = map tanh sq

let saneValue sq = map (fun x -> if Float.is_finite x then x else 0.0) sq
           
let l1,r1 = mks 0.0 60.0 boer
let l2,r2 = mks 55.0 80.0 (mkBrown 5000.0)
let l2a,r2a = mks 100.0 10.0 (mkBrown 2000.0)
let l3,r3 = mks 120.0 60.0 (mkDigi ()) 
let l4,r4 = mks 180.0 25.0 stutter 
let l5,r5 = mks 200.0 5.0 (mkDigi())
let l6,r6 = mks 200.0 60.0 (mkDigi ())
let l7,r7 = mks 260.0 15.0 (noiseL,noiseR)
let l8,_ = mks 275.0 135.0 boer2
let _,r8 = mks 275.0 135.0 zigzag
let (l10,_) = mks 410.0 60.0 (mkDigi ())
let (_,r10) = mks 440.0 60.0 loopsy
let l11,r11 = mks 500.0 60.0 zigzag
let l12,r12 = mks 500.0 60.0 loopsy
let l13,r13 = mks 550.0 120.0 boer
let l14,r14 = mks 670.0 180.0 (mkBrown 200.0)
let l15,r15 = mks 620.0 200.0 boer
let stl,str = mks 820.0 30.0 ending

(** TESTING ***)
            
let test f =
  let l1,r1 = mks 0.0 900.0 (makeStereo f) in
  let scoreL = playScore (mkScore [l1]) in
  let scoreR = playScore (mkScore [r1]) in
  Jack.playSeqs 8 Process.sample_rate [effect (masterClock) scoreL |> hardClip |> saneValue ;scoreR |> hardClip |> saneValue]

let () =
  test mkBoermanFading

       (*** END OF TESTING ***)
    
    
(*     
the real piece:
      
let scoreL = playScore (mkScore [l1;l2;l2a;l3;l4;l5;l6;l7;l8;l10;l11;l12;l13;l14;l15])
let scoreR = playScore (mkScore [r1;r2;r2a;r3;r4;r5;r6;r7;r8;r10;r11;r12;r13;r14;r15])


            
let () = 
  Jack.playSeqs 8 Process.sample_rate [effect (masterClock) scoreL |> hardClip |> saneValue ;scoreR |> hardClip |> saneValue]

 *)
