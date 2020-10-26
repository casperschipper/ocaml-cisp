open Cisp
open Seq
(*open Format *)
   
let msec = map sec 

let samplesize = 0.1



let voiceWithInputs voicef inputs =
  List.fold_left (fun acc inpt -> (voicef inpt) +.~ acc) (st 0.0) inputs 
  

              
let makeVoice channelN =
  let pitch = tmd (st samplesize) (ch [|0.5;1.0;2.0;0.25;4.0;1.25;0.75|] |> hold (seq [2;3;2;1;4])) in
  let baseline = walk 0.0 pitch |> map (wrapf 0.0 (sec samplesize)) in
  let entrydelay = (st samplesize) in
  let offset = (rv (st 0) (st 50) |> hold (seq [3;2;2;3;5]) |> floatify) *.~ (st <| sec samplesize) in
  let randomSteppy = tmd entrydelay offset in
  let jumpyLine = baseline +.~ randomSteppy in
  
  let buffer = Array.make (sec 5.0 |> Int.of_float) 0.0 in 
  let input = Process.inputSeq channelN in
  let hpf = bhpf_static 100.0  0.9 input in
  let percEnv () =
   decayPulse 0.9999 (pulsegen (rvf (st 0.1) (st 5.0))) in
  let myReader () = (indexCub buffer (jumpyLine)) *.~ percEnv () in
  
  let writer = write buffer (countTill (cap buffer)) hpf in
  let joined = effect writer (myReader ()) in
  joined


let mkBoerman nInput =
  let place =
    (seq [0.0;4.0 |> sec]) +.~ (rvf (st 0.0) (st 3.0))
  in
  let dura =
   (ch [|4.0;8.0;2.0;5.0;3.0;4.5;5.5;4.5|])
  in       
  let myLineTest () =
    tline dura place +.~ (rvf (st 0.0) (st 2.0))
  in
  let buffer = Array.make (sec 5.0 |> Int.of_float) 0.0 in 
  let input = Process.inputSeq nInput in
  let input_osc = input in
  let hpf = bhpf_static 100.0 0.9 input_osc *.~ (st 2.0) |> map (clip (-1.0) 1.0) in
  let writer = write buffer (countTill <| cap buffer) hpf in
  let myReader () = indexCub buffer (myLineTest ()) in
  
  let joined = effectSync writer (myReader ()) in
  joined 

let sec1SqL = voiceWithInputs makeVoice (rangei 0 3 |> toList)
let sec1SqR = voiceWithInputs makeVoice (rangei 4 7 |> toList)

let sec2SqL = voiceWithInputs mkBoerman (rangei 0 3 |> toList)
let sec2SqR = voiceWithInputs mkBoerman (rangei 4 7 |> toList)

let sec1 = mkSection 0 (60.0 |> seci) sec1SqL
let sec2 = mkSection (30.0 |> seci) (60.0 |> seci) sec2SqL
let sec3 = mkSection (15.0 |> seci) (60.0 |> seci) sec1SqR
let sec4 = mkSection (60.0 |> seci) (60.0 |> seci) sec2SqR

let scoreL = playScore (mkScore [sec1;sec2])
let scoreR = playScore (mkScore [sec3;sec4])

let () = 
  Jack.playSeqs 8 Process.sample_rate [effect (masterClock) scoreL;scoreR]
    

