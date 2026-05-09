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

  
let percEnv () =
   decayPulse 0.999 (pulsegen (st 20.0))

let oscil = osc (st 440.0) 

let mySec1 = mkSection 10 441000 (osc (st 4400.0) *.~ percEnv ())
let mySec2 = mkSection 44100 88000 (osc (st 1000.0))
let mySec3 = mkSection 66000 50000 (osc (st 1200.0))

(*let manySec = range 1.0 100.0 |> map ((/.) 10.0) |> map seci |> map (fun startT -> mkSection startT (rvi 1 1000000) (osc (st (mtof (rvfi 40.0 120.0))))) |> toList
 *)      
let myScore = mkScore ([mySec1;mySec2;mySec3])



let myOut = playScore myScore
  
let singleton a =
  [a]

let testScheduler () =
  myOut |> take 30 |> toList |> List.map (fun x -> printf "%f\t" x) 
              
let () =
  (* let buffer = Array.make (sec 5.0 |> Int.of_float) 0.0 in 
  let input = Process.inputSeq 0 in
  let hpf = bhpf_static  50.0  0.9 input in
  let myReader () = (indexCub buffer (jumpyLine)) *.~ env () in
  let timefied = effectSync masterClock (myReader ()) in
  let writer = write buffer (countTill (cap buffer)) hpf in
  let joined = effectSync writer timefied in*)
  (*let _ = testScheduler () in ()*)
  Jack.playSeqs 0 Process.sample_rate [myOut *.~ (st 0.01);myOut *.~ (st 0.01)]

