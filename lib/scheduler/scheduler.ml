(* schedular allows to schedule parallel streams of any type.
   The unit is a timedsection, which has a timedsection that defines its content and a starting sample.

   Schedular's schedule to an internal sample clock.

   To play back any type, one has to define the properties (monoid):

   - empty event, for example, MidiSilence or 0.0
   - merge two into one stream

   These have been provided for float streams and midi event streams.
*)

(* section has a start point and a sequence of sounds *)

type 'a timedSection = Section of { startSample : int; seq : 'a Seq.t }
(* finite, has an end *)

let mkSectionOfInf start duration (sq : 'a Infseq.t) =
  let truncated = Infseq.take duration sq in
  Section { startSample = start; seq = truncated }

let mkSection start duration seq =
  Section { startSample = start; seq = Cisp.take duration seq }

let compareSection (Section sectA) (Section sectB) =
  compare sectA.startSample sectB.startSample

type 'a playingSection = PlayingSection of { seq : 'a Seq.t }
type 'a score = Score of 'a timedSection Sorted.t

let toPlay (Section section) = PlayingSection { seq = section.seq }

type 'a t =
  | SectionScheduler of {
      score : 'a timedSection Sorted.t;
      playingScore : 'a timedSection Sorted.t;
      now : int;
      currentSecs : 'a playingSection list;
      currentOut : 'a;
    }

let schedWithOffset (SectionScheduler score) offsetInSmps =
  SectionScheduler { score with now = offsetInSmps }

let schedulerOfScore empty (Score sortedTimedSecs) =
  SectionScheduler
    {
      playingScore = sortedTimedSecs;
      score = sortedTimedSecs;
      now = 0;
      currentSecs = [];
      currentOut = empty;
    }

let mkScore sectionLst = Score (Sorted.mkSorted compareSection sectionLst)

(* this is for seqs of floats *)
let mappend_float_sq (sum, tails) (PlayingSection playingSeq) =
  (* this takes the heads of secs sums them, and updates the remaining part in the state *)
  match playingSeq.seq () with
  | Nil -> (sum, tails)
  | Cons (x, tail) -> (sum +. x, PlayingSection { seq = tail } :: tails)

let mappend_midi (bundle, tails) (PlayingSection midiEvtSq) =
  match midiEvtSq.seq () with
  | Seq.Nil -> (bundle, tails)
  | Seq.Cons (midiEvt, tail) ->
      (Midi.addToBundle bundle midiEvt, PlayingSection { seq = tail } :: tails)

let merge mergefunc (merged, tails) (PlayingSection sq) =
  match sq.seq () with
  | Seq.Nil -> (merged, tails)
  | Seq.Cons (head, tail) ->
      (mergefunc merged head, PlayingSection { seq = tail } :: tails)

let updateScheduler (SectionScheduler scheduler) merge empty =
  let newCurrentSecs, future =
    scheduler.playingScore
    |> Sorted.mozesSorted (fun (Section e) -> e.startSample <= scheduler.now)
    |> Cisp.mapFst (fun (Sorted.Sorted playableEvts) ->
           List.map toPlay playableEvts)
  in
  let currentSects = newCurrentSecs @ scheduler.currentSecs in
  let out, newPlayingSecs = List.fold_left merge (empty, []) currentSects in
  match (future, newPlayingSecs) with
  (* to deal with reset, if there is no future, then we reset the score *)
  | Sorted.Sorted [], [] ->
      SectionScheduler
        {
          scheduler with
          playingScore = scheduler.score;
          now = 0;
          currentSecs = [];
          currentOut = out;
        }
  | Sorted.Sorted futureEvts, _ ->
      SectionScheduler
        {
          scheduler with
          playingScore = Sorted futureEvts;
          now = scheduler.now + 1;
          currentSecs = newPlayingSecs;
          currentOut = out;
        }

let printTimedSectionLst timedSectionList =
  let open Format in
  let printSection i (Section s) =
    printf "-section nr %i\nstartSample %i\n" i s.startSample
  in
  List.iteri printSection timedSectionList

let printScheduler (SectionScheduler sch) =
  let open Format in
  printf "\n** now: %i **\n score=\n" sch.now;
  printTimedSectionLst (Sorted.sortedAsList sch.score);
  printf "currently playing %i" (List.length sch.currentSecs)

let evaluateScheduler (SectionScheduler scheduler) = scheduler.currentOut

(* let playScore score =
   Cisp.simpleRecursive (schedulerOfScore score) updateScheduler evaluateScheduler*)
