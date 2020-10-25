open Cisp
open Seq
open Format 
   
let sec s = !Process.sample_rate *. s

let seci s = !Process.sample_rate *. s |> Int.of_float

let msec = map sec 

let samplesize = 0.1

let pitch = tmd (st samplesize) (ch [|0.5;1.0;2.0;0.25;4.0;1.25;0.75|] |> hold (seq [2;3;2;1;4]))   
let baseline = walk 0.0 pitch |> map (wrapf 0.0 (sec samplesize))
let entrydelay = (st samplesize)
let offset = (rv (st 0) (st 50) |> hold (seq [3;2;2;3;5]) |> floatify) *.~ (st <| sec samplesize) 
let randomSteppy = tmd entrydelay offset
let jumpyLine = baseline +.~ randomSteppy

let decay fb inSq =
  let rec aux prev inSq () = 
  match inSq () with
  | Cons(hd, tail) -> Cons (hd +. prev, aux (hd *. fb) tail)
  | Nil -> Nil
  in
  aux 0.0 inSq 

let env () =
  tline (seq [0.1 -. 0.00001;0.00001]) (seq [1.0;0.0]) |> map (fun x -> x *. x |> clip 0.0 1.0) 

let oscil = osc (st 440.0) 


    
type timedSection
  = TimedSection of { startSample : int
                    ; duration : int
                    ; seq : float Seq.t} 

let mkSection start duration seq =
  TimedSection { startSample = start ; duration = duration ; seq = seq }
    
                  
let addStreo (seq1L,seq1R) (seq2L,seq2R) =
  (seq1L +.~ seq2L,seq1R +.~ seq2R)
                  
let compareSection (TimedSection sectA) (TimedSection sectB) =
  compare (sectA.startSample) (sectB.startSample)
                  
type playingSection
  = PlayingSection of { endSample : int
                      ; seq : float Seq.t
                      }

let printPlayingSect (PlayingSection s) =
  printf "end %i\n" s.endSample 
      
  

                    
type score =
  Score of timedSection sorted
                    
let toPlay now (TimedSection section) =
  PlayingSection { endSample = now + section.duration
                 ; seq = section.seq }
  
type sectionScheduler =
  SectionScheduler of { score : timedSection sorted
                      ; now : int
                      ; currentSecs : playingSection list
                      ; currentOut : float
                      }

let schedulerOfScore (Score sortedTimedSecs) =
  SectionScheduler { score = sortedTimedSecs
                   ; now = 0
                   ; currentSecs = []
                   ; currentOut = 0.0 }

(*
float t list -> (float, float t list)

List.fold_left (fun x acc -> 

 *)

let mkScore sectionLst =
  Score (mkSorted compareSection sectionLst)

let updateScheduler (SectionScheduler scheduler) =
  let dropFinished playingSects =
    List.filter (fun (PlayingSection playing) -> playing.endSample > scheduler.now)  playingSects
  in
  let (newCurrentSecs, future) = (* secs that have their start now, turn them into playingsecs *)
    (*scheduler.score |> sortedAsList |> List.iter (fun (TimedSection sect) -> printf "%i sectstart: i\n" sect.startSample);    *)
    scheduler.score
    |> mozesSorted (fun (TimedSection e) ->
           e.startSample <= scheduler.now)
    |> mapFst (fun (Sorted playableEvts) -> List.map (toPlay scheduler.now) playableEvts)
  in
  (*let _ = printf "future length = %i\n new current seqs %i \n" (List.length (future |> sortedAsList)) (List.length newCurrentSecs) in*)
  let currentSects = newCurrentSecs @ (dropFinished scheduler.currentSecs) in
  let f (sum,tails) (PlayingSection playingSeq) = (* this takes the heads of secs sums them, and updates the remaining part in the state *)
    match playingSeq.seq () with
    | Nil -> (sum +. 0.0, tails)
    | Cons(x,tail) -> (sum +. x, (PlayingSection { playingSeq with seq = tail })::tails)                  
  in
  let (out,newPlayingSecs) =
    List.fold_left f (0.0,[]) currentSects
  in 
  (SectionScheduler
     { score = future
     ; now = scheduler.now + 1
     ; currentSecs = newPlayingSecs
     ; currentOut = out })

let printTimedSectionLst timedSectionList =
  let printSection i (TimedSection s) =
    printf "-section nr %i\nstartSample %i\nduration %i\n" i s.startSample s.duration
  in
  List.iteri printSection timedSectionList
  
  
let printScheduler (SectionScheduler sch) =
  printf "\n** now: %i **\n score=\n" sch.now;
  printTimedSectionLst (sortedAsList sch.score);
  printf "playing: \n";
  List.iter printPlayingSect sch.currentSecs
  
  
let evaluateScheduler (SectionScheduler scheduler) =
  (*printScheduler (SectionScheduler scheduler);*)
  scheduler.currentOut
    
let playScore score =
  simpleRecursive
    ((schedulerOfScore score))
    updateScheduler
    evaluateScheduler

let mySec1 = mkSection 10 44100 (osc (st 440.0))
let mySec2 = mkSection 500 500 (osc (st 1000.0))
let mySec3 = mkSection 10000 5000 (osc (st 1200.0))

let manySec = range 1.0 100.0 |> map ((/.) 10.0) |> map seci |> map (fun startT -> mkSection startT (rvi 1 1000000) (osc (st (rvfi 300.0 20000.0)))) |> toList
           
(*let myScore = mkScore ([mySec1;mySec2;mySec3])*)



let myOut = playScore (mkScore manySec)
  
let singleton a =
  [a]

let testScheduler () =
  myOut |> take 30 |> toList |> List.map (fun x -> printf "%f\t" x) 
              
let () =
  (* let buffer = Array.make (sec 5.0 |> Int.of_float) 0.0 in 
  let input = Process.inputSeq 0 in
  let hpf = bhpf_static  50.0  0.9 input in
  let myReader () = (indexCub buffer (jumpyLine)) *.~ env () in
  let timefied = effect masterClock (myReader ()) in
  let writer = write buffer (countTill (cap buffer)) hpf in
  let joined = effect writer timefied in*)
  (*let _ = testScheduler () in ()*)
  Jack.playSeqs 0 Process.sample_rate [myOut *.~ (st 0.01);myOut *.~ (st 0.01)]

