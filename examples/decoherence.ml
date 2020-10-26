open Cisp
open Seq
open Format 
   
let msec = map sec 

let samplesize = 0.1

let pitch = tmd (st samplesize) (ch [|0.5;1.0;2.0;0.25;4.0;1.25;0.75|] |> hold (seq [2;3;2;1;4]))   
let baseline = walk 0.0 pitch |> map (wrapf 0.0 (sec samplesize))
let entrydelay = (st samplesize)
let offset = (rv (st 0) (st 50) |> hold (seq [3;2;2;3;5]) |> floatify) *.~ (st <| sec samplesize) 
let randomSteppy = tmd entrydelay offset
let jumpyLine = baseline +.~ randomSteppy

 

let oscil = osc (st 440.0) 

let mySec1 = mkSection 10 441000 (osc (st 4400.0) *.~ percEnv ())
let mySec2 = mkSection 44100 88000 (osc (st 1000.0))
let mySec3 = mkSection 66000 50000 (osc (st 1200.0))
   
let myScore = mkScore ([mySec1;mySec2;mySec3])



let myOut = playScore myScore

let rangei a b =
  let rec aux a b () =
    if a = b then
      Cons(a, fun () -> Nil)
    else
      Cons(a,aux (a+1) b)
  in
  if b < a then
    aux b a
  else
    aux a b

let voiceWithInputs voicef inputs =
  List.fold_left (fun acc inpt -> (voicef inpt) +.~ acc) (st 0.0) inputs 
  
let testScheduler () =
  myOut |> take 30 |> toList |> List.map (fun x -> printf "%f\t" x) 
              
let makeVoice channelN =
  let buffer = Array.make (sec 5.0 |> Int.of_float) 0.0 in 
  let input = Process.inputSeq channelN in
  let hpf = bhpf_static 100.0  0.9 input in
  let percEnv () =
   decayPulse 0.9999 (pulsegen (rvf (st 0.1) (st 5.0))) in
  let myReader () = (indexCub buffer (jumpyLine)) *.~ percEnv () in
  let timefied = effect masterClock (myReader ()) in
  let writer = write buffer (countTill (cap buffer)) hpf in
  let joined = effect writer timefied in
  joined

let () = 
  Jack.playSeqs 8 Process.sample_rate
    [ voiceWithInputs makeVoice (rangei 0 3 |> toList); voiceWithInputs makeVoice (rangei 4 7 |> toList)]

