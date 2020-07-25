open Cisp
open Seq

type midiValue = MidiVal of int

type velocity = Velo of int

type pitch = Pitch of int

type midiChannel = MidiCh of int

type controller = MidiCtrl of int

type deltaT = Samps of int

type midiEvent =
  | NoteEvent of midiChannel * pitch * velocity * deltaT
  | ControlEvent of midiChannel * controller * midiValue
  | SilenceEvent

let mkMidiValue v =
  if v < 0 || v > 127 then Error "out of range midi value" else Ok (MidiVal v)

let mkChannel ch =
  if ch < 1 || ch > 16 then Error "channel should be between 1 and 16"
  else Ok (MidiCh (ch - 1))

let mkChannelClip ch =
  let clipped = clip 0 15 (ch - 1) in
  MidiCh clipped

let mkPitchClip p =
  let clipped = clip 0 127 p in
  Pitch clipped

let mkVelocityClip v =
  let clipped = clip 0 127 v in
  Velo clipped

let mkSampsClip s = if s < 0 then Samps 0 else Samps s

let mkPitch p =
  if p < 0 || p > 127 then Error "pitch out of range" else Ok (Pitch p)

let mkVelocity v =
  if v < 0 || v > 127 then Error "velocity out of range" else Ok (Velo v)

let mkSamps s =
  if s < 0 then Error "deltaT cannot be negative" else Ok (Samps s)

let mapResult4 f a b c d =
  match (a, b, c, d) with
  | Ok a', Ok b', Ok c', Ok d' -> Ok (f a' b' c' d')
  | Error a', _, _, _ -> Error a'
  | _, Error b', _, _ -> Error b'
  | _, _, Error c', _ -> Error c'
  | _, _, _, Error d' -> Error d'

let mkNote c p v d =
  mapResult4
    (fun ch pi ve dt -> NoteEvent (ch, pi, ve, dt))
    (mkChannel c) (mkPitch p) (mkVelocity v) (mkSamps d)

let mkNoteClip c p v d =
  let ch = mkChannelClip c in
  let pi = mkPitchClip p in
  let ve = mkVelocityClip v in
  let dt = mkSampsClip d in
  NoteEvent (ch, pi, ve, dt)

let mapOverPitch f evt =
  (* output of f is clipped to garentee valid pitch *)
  match evt with
  | NoteEvent (c, Pitch p, v, d) -> NoteEvent (c, mkPitchClip (f p), v, d)
  | other -> other

let mapOverCh f evt =
  (* output of f is clipped to garentee valid pitch *)
  match evt with
  | NoteEvent (MidiCh c, p, v, d) -> NoteEvent (mkChannelClip (f c), p, v, d)
  | other -> other

let mapOverVelo f evt =
  (* output of f is clipped to garentee valid pitch *)
  match evt with
  | NoteEvent (c, p, Velo v, d) -> NoteEvent (c, p, mkVelocityClip (f v), d)
  | other -> other

let rec withPitch pitchSq sq () =
  match sq () with
  | Cons (NoteEvent (c, _, v, d), tl) -> (
    match pitchSq () with
    | Cons (pitch, ptl) ->
        let p = mkPitchClip pitch in
        Cons (NoteEvent (c, p, v, d), withPitch ptl tl)
    | Nil -> Nil )
  | Cons (event, tl) -> Cons (event, withPitch pitchSq tl)
  | Nil -> Nil

let rec withDur durSq sq () =
  match sq () with
  | Cons (NoteEvent (c, p, v, _), tl) -> (
    match durSq () with
    | Cons (dur, dtl) ->
        let d = mkSampsClip dur in
        Cons (NoteEvent (c, p, v, d), withDur dtl tl)
    | Nil -> Nil )
  | Cons (event, tl) -> Cons (event, withDur durSq tl)
  | Nil -> Nil

let rec withChan chanSq sq () =
  match sq () with
  | Cons (NoteEvent (_, p, v, d), tl) -> (
    match chanSq () with
    | Cons (chan, chtl) ->
        let c = mkChannelClip chan in
        Cons (NoteEvent (c, p, v, d), withChan chtl tl)
    | Nil -> Nil )
  | Cons (event, tl) -> Cons (event, withChan chanSq tl)
  | Nil -> Nil

let rec withVelo veloSq sq () =
  match sq () with
  | Cons (NoteEvent (c, p, _, d), tl) -> (
    match veloSq () with
    | Cons (velo, vtl) ->
        let v = mkVelocityClip velo in
        Cons (NoteEvent (c, p, v, d), withVelo vtl tl)
    | Nil -> Nil )
  | Cons (event, tl) -> Cons (event, withVelo veloSq tl)
  | Nil -> Nil

type midiMessage =
  | NoteOn of midiChannel * pitch * velocity
  | NoteOff of midiChannel * pitch * velocity
  | Control of midiChannel * controller * midiValue
  | ClockTick
  | ClockStart
  | ClockStop
  | MidiSilence

let mapOverMidiPitch f msg =
  match msg with
  | NoteOn (ch, Pitch p, v) -> NoteOn (ch, Pitch (f p |> clip 0 127), v)
  | NoteOff (ch, Pitch p, v) -> NoteOff (ch, Pitch (f p |> clip 0 127), v)
  | other -> other

let prepend prefix str = prefix ^ str

let midiToString = function
  | NoteOn (MidiCh ch, Pitch p, Velo v) ->
      List.map Int.to_string [ch; p; v]
      |> String.concat "-" |> prepend "NoteOn "
  | NoteOff (MidiCh ch, Pitch p, Velo v) ->
      List.map Int.to_string [ch; p; v]
      |> String.concat "-" |> prepend "NoteOff "
  | Control (MidiCh ch, MidiCtrl ctrl, MidiVal v) ->
      List.map Int.to_string [ch; ctrl; v]
      |> String.concat "-" |> prepend "Control"
  | ClockTick -> "midi-rt-tick"
  | ClockStart -> "midi-rt-start"
  | ClockStop -> "midi-rt-stop"
  | MidiSilence -> "silence"

let toRaw midiMessage =
  match midiMessage with
  | NoteOn (MidiCh ch, Pitch p, Velo v) -> (0x90 lor ch, p, v)
  | NoteOff (MidiCh ch, Pitch p, Velo v) -> (0x80 lor ch, p, v)
  | Control (MidiCh ch, MidiCtrl ctrl, MidiVal v) -> (0xb0 lor ch, ctrl, v)
  | ClockTick -> (0xf8, 0, 0)
  | ClockStart -> (0xf6, 0, 0)
  | ClockStop -> (0xfa, 0, 0)
  | MidiSilence -> (0, 0, 0)

let chFromByte byte = MidiCh (byte land 0x0f)

let fromRaw (status, data1, data2) =
  let statusByte = status land 0xf0 in
  match statusByte with
  | 0x90 ->
      if data2 > 0 (* Zero velocity note on, lets not *) then
        NoteOn (chFromByte status, Pitch data1, Velo data2)
      else NoteOff (chFromByte status, Pitch data1, Velo data2)
  | 0x80 -> NoteOff (chFromByte status, Pitch data1, Velo data2)
  | 0xb0 -> Control (chFromByte status, MidiCtrl data1, MidiVal data2)
  | 0xf8 -> ClockTick
  | 0xf6 -> ClockStart
  | 0xfa -> ClockStop
  | _ -> MidiSilence

(*
let defaultTranslator  = {
    onNoteOn = fun NoteOn (_,_,_) -> SilenceEvent  *)

let printRaw (status, data1, data2) =
  if status != 0 then
    let () = print_int status ; print_int data1 ; print_int data2 in
    ()
  else ()

(* intersperce a Seq with silence 
 * M...M...M...M...  
 * *)
let withInterval interval fillerEvent sq =
  let ctrl = zip sq interval in
  concatMap (fun (src, Samps n) () -> Cons (src, repeat n fillerEvent)) ctrl

(* does not work: *
let intervalNotesOnly interval sq =
  let rec aux interval sq curr () =
    if curr < 1 then
      match interval () with
      | Nil -> Nil
      | Cons (newinterval, itl) -> (
        match sq () with
        | Cons (NoteEvent (c, p, v, d), tail) ->
            Cons (NoteEvent (c, p, v, d), aux itl tail newinterval)
        | any -> any )
    else
      match sq () with
      | Cons (NoteEvent (_, _, _, _), _) ->
          Cons (SilenceEvent, aux interval sq (curr - 1))
      | Cons (evt, tail) -> Cons (evt, aux interval tail curr)
      | Nil -> Nil
  in
  aux interval sq 0
   *)

type ordering = Greater | Smaller | Equal

let rec insertBy cmp v sq () =
  match sq () with
  | Nil -> Cons (v, fun () -> Nil)
  | Cons (h, tl) -> (
    match cmp h v with
    | Greater -> Cons (h, insertBy cmp v tl)
    | _ -> Cons (v, sq) )

let insertMidiEvent evt sq =
  insertBy
    (fun (t1, _) (t2, _) ->
      if t1 > t2 then Greater else if t1 < t2 then Smaller else Equal)
    evt sq

let rec sequenceRelative start sq () =
  match sq () with
  | Cons ((d, event), tl) ->
      Cons ((start, event), sequenceRelative (start + d) tl)
  | Nil -> Nil

type timedMidiEvent = int * midiMessage

let print_midi_msg msg =
  midiToString msg |> print_string ;
  print_newline ()

let print_timed_midi_event (t, msg) =
  let () =
    print_string "t=" ;
    print_int t ;
    print_string "|" ;
    print_midi_msg msg ;
    print_newline ()
  in
  ()

type midiSerializer =
  { now: int
  ; pendingNoteOffs: timedMidiEvent FQueue.t
  ; deferred: midiMessage FQueue.t }

let initSerializer : midiSerializer =
  {now= 0; deferred= FQueue.empty; pendingNoteOffs= FQueue.empty}

let peek = FQueue.peek

let dequeue = FQueue.dequeue

let enqueue = FQueue.enqueue

let enqueueOnlyNotes v q =
  match v with MidiSilence -> q | any -> FQueue.enqueue any q

let print_state m label =
  let now = m.now in
  let deferred = FQueue.to_list m.deferred in
  let pendingNoteOffs = FQueue.to_list m.pendingNoteOffs in
  print_string label ;
  print_newline () ;
  print_string "now: " ;
  print_int now ;
  print_string
    (" pending offs: " ^ Int.to_string (List.length pendingNoteOffs) ^ "-") ;
  List.iter print_timed_midi_event pendingNoteOffs ;
  print_string (" deferred: " ^ Int.to_string (List.length deferred) ^ "-") ;
  List.iter print_midi_msg deferred ;
  print_newline ()

let getPending newEvt m =
  (* let () = print_state m "pending" in *)
  let pending = peek m.pendingNoteOffs in
  match pending with
  | Some (t, evt) ->
      if t <= m.now then
        ( evt
        , { m with
            pendingNoteOffs= dequeue m.pendingNoteOffs
          ; deferred= enqueueOnlyNotes newEvt m.deferred } )
      else (newEvt, m)
  | None -> (newEvt, m)

let getDeferred newEvt m =
  (* let () = print_state m "deferred" in *)
  let d = peek m.deferred in
  match d with
  | Some devt ->
      (devt, {m with deferred= enqueueOnlyNotes newEvt (dequeue m.deferred)})
  | None -> (newEvt, m)

let handleMidiEvent midiEvt m =
  match midiEvt with
  | NoteEvent (ch, p, v, Samps dura) ->
      let noteOff = (m.now + dura, NoteOff (ch, p, v)) in
      ( NoteOn (ch, p, v)
      , {m with pendingNoteOffs= enqueue noteOff m.pendingNoteOffs} )
  | ControlEvent (ch, ctrl, v) -> (Control (ch, ctrl, v), m)
  | SilenceEvent -> (MidiSilence, m)

let nowPlusOne evt m = ({m with now= m.now + 1}, evt)

let updateMidi midiEvt m =
  (* waterfall event through a bunch of state changing functions *)
  let ( ||> ) (evt, m) f = f evt m in
  (* 
    - check for pending note offs, if so, pass it on, store new event in deferred queue
    - then deferred notes , if present, pass on and store current event in queue
    - if there are no pending note-offs or deferred, note is played immediately
    - time is increased by one
  *)
  let state, evt =
    (midiEvt, m) ||> handleMidiEvent ||> getPending ||> getDeferred
    ||> nowPlusOne
  in
  (evt, state)

let serialize midi =
  let startM = initSerializer in
  let rec aux msg_sq model () =
    match msg_sq () with
    | Nil -> Nil
    | Cons (evt, tl) ->
        let e, state = updateMidi evt model in
        (* let () = print_string ("current event: " ^ midiToString e ^ "\n") in *)
        Cons (e, aux tl state)
  in
  aux midi startM

let midiPitch pitch =
  if pitch < 0 then Error ("pitch too low: " ^ Int.to_string pitch ^ "\n")
  else if pitch > 127 then
    Error ("pitch too high: " ^ Int.to_string pitch ^ "\n")
  else Ok pitch

let midiChannel ch =
  if ch < 1 then Error ("channel cannot be negative " ^ "\n")
  else if ch > 16 then Error "channel cannot be higher than 16"
  else Ok (ch - 1)

(*
let _ =
  let proc = Process.ofSeq fish in
  Jack.play 0 Process.sample_rate [proc]*)

(* Midi Out *)

let scale =
  let amp = ch [|100; 90; 100|] in
  let timing = lift rv 100 400 in
  map
    (fun (i, amp, time) ->
      let step = i mod 12 in
      mkNote 1 ((step * 7) + 36) amp time)
    (zip3 count amp timing)

let controller midiRef midiCh midiCtrl =
  let rec aux previous () =
    let msg = !midiRef |> fromRaw in
    match msg with
    | Control (MidiCh ch, MidiCtrl ctrl, MidiVal v)
      when ch = midiCh && ctrl = midiCtrl ->
        Cons (v, aux v)
    | _ -> Cons (previous, aux previous)
  in
  aux 0

let pitchVelo midiRef midiCh =
  let rec aux previous () =
    let msg = !midiRef |> fromRaw in
    match msg with
    | NoteOn (MidiCh ch, Pitch p, Velo v) when ch = midiCh ->
        Cons ((p, v), aux previous)
    | _ -> Cons (previous, aux previous)
  in
  aux (0, 0)

let rec difference sq start () =
  match sq () with
  | Nil -> Nil
  | Cons (h, tl) -> Cons (h - start, difference tl h)

let onlyPitch sq = map fst sq

let onlyVelo sq = map snd sq

let seconds s = Samps (44100.0 *. s |> Int.of_float)

let timing = seconds 0.01 |> st

let rec ofRef rf () = Cons (!rf, ofRef rf)

let testSequence =
  withInterval (st (Samps 4)) MidiSilence
    (map (fun i -> NoteOn (MidiCh 1, Pitch (60 + (i mod 12)), Velo 100)) count)

let justSilence = ref (st (toRaw MidiSilence))

(* combine two Seq's: a, b, a, b, a, b etc.. *)
let rec interleave xs ys () =
  match (xs (), ys ()) with
  | Nil, Nil -> Nil
  | xs', Nil -> xs'
  | Nil, ys' -> ys'
  | Cons (x, xtl), Cons (y, ytl) ->
      Cons (x, fun () -> Cons (y, interleave xtl ytl))

(*
Similar to interleave, but now you can provide in which pattern the seqs needs to be combined.
For example if the pattern is 1 0 0 1 0 0
and a = 1,2,3,4
and b = 11,12,13,14
then you will get
1, 11, 12, 2, 13, 14
This is different from zipWith3, since there are no values thrown away
 *)
let rec weavePattern pattern xs ys () =
  match (xs (), ys ()) with
  | Nil, Nil -> Nil
  | xs, Nil -> xs
  | Nil, ys -> ys
  | Cons (x, xtl), Cons (y, ytl) -> (
    match pattern () with
    | Cons (true, ptl) -> Cons (x, weavePattern ptl xtl ys)
    | Cons (false, ptl) -> Cons (y, weavePattern ptl xs ytl)
    | Nil -> Nil )

(* this maps midi input msg to an output msg (raw midi) *)
let midiInputTestFun input =
  (* this translates input into boolean trigger *)
  let inputTrigger =
    map
      (fun midiMsg ->
        match midiMsg with NoteOn (_, _, _) -> true | _ -> false)
      input
  in
  (* a metre boolean mask that triggers notes or rests in the pattern *)
  let metre =
    seq [2; 3; 2; 1] |> map (fun n () -> Cons (false, repeat n true)) |> concat
  in
  let pitchPattern =
    transpose
      (ofList
         [ seq [60; 64; 67; 72; 76; 79; 84]
         ; seq [53; 60; 67; 74; 81]
         ; hold (seq [3; 4; 5]) (seq [60; 67; 60; 72; 48; 36])
         ; seq (List.rev [60; 62; 64; 67; 69; 71; 72; 74; 76]) ])
    |> concat
  in
  (* dummy event *)
  let defaultEvts =
    st (NoteEvent (MidiCh 1, Pitch 60, Velo 100, seconds 0.2))
  in
  (* apply metre to default event, use Silence as filler *)
  let maskedEvts = weavePattern metre defaultEvts (st SilenceEvent) in
  inputTrigger
  |> (fun pattern -> weavePattern pattern maskedEvts (st SilenceEvent))
  |> withPitch
       ( loop (seq [3; 2; 3; 1]) (seq [2; 2; 3]) pitchPattern
       |> concat
       |> hold (st 1) )
  |> withDur (ch [|44100; 88200; 5000; 10000; 500|])
  |> withChan (st 1)
  |> withVelo (st 100)
  |> serialize |> map toRaw

let run () =
  let state = justSilence in
  (* the sq state var *)
  let inputRef = ref MidiSilence in
  let () = state := ofRef inputRef |> midiInputTestFun in
  let callback input =
    (* Slightly troublesome, this looks like a pure function (a -> b) but reads and writes to references *)
    let out =
      match !state () with
      | Cons (curr, tl) ->
          let () = state := tl in
          curr
      | Nil -> (0, 0, 0)
    in
    let () = inputRef := fromRaw input in
    out
  in
  JackMidi.playMidi callback samplerate
