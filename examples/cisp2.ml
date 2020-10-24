open Cisp
open Seq
(* open Format   *)
   
let sec s = !Process.sample_rate *. s

let msec = map sec 

let samplesize = 0.1

let pitch = tmd (st samplesize) (ch [|0.5;1.0;2.0;0.25;4.0|] |> hold (seq [2;3;2;1;4]))   
let baseline = walk 0.0 pitch |> map (wrapf 0.0 (sec samplesize))
let entrydelay = (st samplesize)
let step () = tmd (st 0.6) (rv (st 0) (st 50)) 
let offset = (boundedWalk 0 (step ()) (wrap 0 50) |> hold (seq [3;2;2;3;5]) |> floatify) *.~ (st <| sec samplesize) 
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
  tline (seq [0.1;0.0]) (seq [1.0;0.0;1.0]) |> map (fun x -> x *. x)

let oscil = osc (st 440.0) 


    
type timedSection
  = TimedSection of { startSample : int
                    ; duration : int
                    ; seqs : float Seq.t list } 

let compareSection (TimedSection sectA) (TimedSection sectB) =
  compare (sectA.startSample) (sectB.startSample)
                  
type playingSection
  = PlayingSection of { endSample : int
                      ; seqs : (float Seq.t) list
                      }

type score =
  Score of timedSection list
                    
let toPlay now (TimedSection section) =
  PlayingSection { endSample = now + section.duration
                 ; seqs = section.seqs }
  
type sectionScheduler =
  SectionScheduler of { score : timedSection list
                      ; now : int
                      ; currentSeqs : playingSection list
                      }

let schedulerOfScore (Score timedSeqs) =
  SectionScheduler { score = timedSeqs
                   ; now = 0
                   ; currentSeqs = [] }
  
let mkScore sectionLst =
  Score (List.sort compareSection sectionLst)

let updateScheduler (SectionScheduler scheduler) =
  let dropFinished playingSects =
    List.filter (fun (PlayingSection playing) -> playing.endSample > scheduler.now)  playingSects
  in
  let (newCurrent, future) =
    scheduler.score
    |> mozesSorted (fun (TimedSection e) -> e.startSample >= scheduler.now)
    |> mapFst (fun es -> List.map (toPlay scheduler.now) es)
  in
  let currentSects = newCurrent @ (dropFinished scheduler.currentSeqs) in
  (SectionScheduler
     { score = future
     ; now = scheduler.now + 1
     ; currentSeqs = currentSects })

let evaluateCurrentChannels (SectionScheduler scheduler) =
  let getChannels (PlayingSection ps) = ps.seqs in
  let currentChannelLsts = List.map getChannels scheduler.currentSeqs in
    list_fold_left1 addChannelLsts currentChannelLsts |> Option.value ~default:(listRepeat 8 (st 0.0))

let eightChannelSilence =
  [st(0.0);st(0.0);st(0.0);st(0.0);st(0.0);st(0.0);st(0.0);st(0.0)]

let playScore score =
  simpleRecursive
    ((schedulerOfScore score))
    updateScheduler
    evaluateCurrentChannels
   
  
let singleton a =
  [a]
              
let () =
  let buffer = Array.make (sec 5.0 |> Int.of_float) 0.0 in 
  let input = Process.inputSeq 0 in
  let myReader () = indexCub buffer (jumpyLine) *.~ env () in
  let timefied = effect masterClock (myReader ()) in
  let writer = write buffer (countTill (cap buffer)) input in
  let joined = effect writer timefied in
  Jack.playSeqs 2 Process.sample_rate [joined +.~ mkLots 6 myReader;mkLots 6 myReader]

