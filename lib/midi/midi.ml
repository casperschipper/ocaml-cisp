open Cisp
open Seq

(**
   I use a reader module to easily extract multiple properties at once from one midi input stream.
  *)
module Reader = struct
  type ('e, 'a) t = Reader of ('e -> 'a)
  (** run the reader *)
  let run = function Reader r -> r
  let map f m = Reader (fun env -> f (run m env))
  let bind f m = Reader (fun env -> run (f (run m env)) env)
  let ( >>= ) m f = bind f m
  let return x = Reader (fun _ -> x)
  let ask () = Reader (fun env -> env)
  (* return the complete reader env *)

  let asks f () = map f (ask ())
  (*  can be used to return something within the reader environment , like a record field *)

  let local f m = Reader (fun env -> run m (f env))

  let apply fp ap =
    fp >>= fun f ->
    ap >>= fun a -> return (f a)

  let ( <*> ) fp ap = apply fp ap
  let map2 f ap bp = apply (map f ap) bp
  let map3 f ap bp cp = map f ap <*> bp <*> cp
  let map4 f ap bp cp dp = map f ap <*> bp <*> cp <*> dp

  module Ops = struct
    let ( >>= ) m f = bind f m
    let ( <*> ) fapp bapp = apply fapp bapp
  end
end

(** Protected newtypes, so we are sure we have valid midi data instead of arbitrary ints *)
type midiValue = MidiVal of int
type velocity = Velo of int
type pitch = Pitch of int
type midiChannel = MidiCh of int

let isChannel (MidiCh a) (MidiCh b) = a == b

type controller = MidiCtrl of int
(** deltaT is always in samples, so we can be sample accurate, there are functions to convert from other time units*)
type deltaT = Samps of int

let oneTick (Samps x) = Samps (x + 1)
let compareSamps (Samps sA) (Samps sB) = sA == sB
let offsetSamps (Samps offset) (Samps dT) = Samps (offset + dT)

(** midievents have a defined duration *)
type midiEvent =
  | NoteEvent of midiChannel * pitch * velocity * deltaT
  | ControlEvent of midiChannel * controller * midiValue
  | SilenceEvent

(** midi bundles are a way to achieve chords or simultanious midi events 
 they need to be sequenced to be played back
*)
type midiBundle = Bundle of midiEvent * midiEvent Seq.t

let bundleAsSeq (Bundle (head, tail)) () = Seq.Cons (head, tail)
let soloBundle note = Bundle (note, Seq.empty)
let silenceBundle = Bundle (SilenceEvent, Seq.empty)

let explicitSilence optEvents =
  let fromOpt opt = match opt with Some x -> x | None -> SilenceEvent in
  Seq.map fromOpt optEvents

let getFirstOfBundle (Bundle (fst, _)) = fst

let addToBundle (Bundle (fst, rest)) note =
  let rec appendToEnd a sq () =
    match sq () with
    | Nil -> Cons (a, fun () -> Nil)
    | Cons (h, rest) -> Cons (h, appendToEnd a rest)
  in
  match fst with
  | SilenceEvent -> Bundle (note, rest)
  | NoteEvent (c, p, v, d) ->
      Bundle (NoteEvent (c, p, v, d), appendToEnd note rest)
  | ControlEvent (ch, co, va) ->
      Bundle (ControlEvent (ch, co, va), appendToEnd note rest)

let addOptToBundle opt bundle =
  match opt with Some evt -> addToBundle bundle evt | None -> bundle

let chord noteSeq =
  match noteSeq () with
  | Cons (note, rest) -> Bundle (note, rest)
  | Nil -> Bundle (SilenceEvent, Seq.empty)

type note = Note of midiChannel * pitch * velocity
type noteEvt = NoteEvt of midiChannel * pitch * velocity * deltaT
type control = Control of midiChannel * controller * midiValue
type midiData = NoteData of note | ControlData of control

let filterNotes midiEvt =
  match midiEvt with
  | NoteEvent (c, p, v, d) -> Some (NoteEvt (c, p, v, d))
  | _ -> None

let filterControl midiEvt =
  match midiEvt with
  | ControlEvent (ch, c, m) -> Some (Control (ch, c, m))
  | _ -> None

(* a (opt b) -> opt (a, b) *)
(*
   let filterEvts midiEvt =
     match midiEvt with
     | NoteEvent (c,p,v,d) -> Some (NoteData (NoteEvt (c,p,v,d)))
     | ControlEvent (ch,c,v) -> Some (ControlData (Control (ch,c,v)))
     | _ -> None*)

(* let getPitch (Note (_,p,_,_)) = p

   let getChannel (Note (c,_,_,_)) = c

   let getVelo (Note (_,_,v,_)) = v

   let getDur (Note (_,_,_,d)) = d *)

let optionToEvent = function Some evt -> evt | None -> SilenceEvent

(** pretty printing *)
let midiEventToString evt =
  match evt with
  | NoteEvent (MidiCh mc, Pitch p, Velo v, Samps s) ->
      [ ("mc: ", mc); ("pitch: ", p); ("velo: ", v); ("samps: ", s) ]
      |> List.map (fun (label, value) -> label ^ Int.to_string value)
      |> String.concat " "
      |> fun str ->
      str ^ "\n" |> fun s -> Some s
  | ControlEvent (MidiCh mc, MidiCtrl c, MidiVal v) ->
      [ ("mc: ", mc); ("ctrl: ", c); ("val: ", v) ]
      |> List.map (fun (label, value) -> label ^ Int.to_string value)
      |> String.concat " "
      |> fun str ->
      str ^ "\n" |> fun s -> Some s
  | SilenceEvent -> Some "---\n"

let printMidiEvent evt =
  match midiEventToString evt with None -> () | Some str -> print_endline str

let printBundle (Bundle (fst, rest)) =
  printMidiEvent fst;
  Seq.iter printMidiEvent rest

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

let pitchOfInt = mkPitchClip
let transposeP (Pitch p) offset = p + offset |> mkPitchClip

let transposePitch trans evt =
  match evt with
  | NoteEvent (ch, Pitch p, v, dur) ->
      NoteEvent (ch, mkPitchClip (p + trans), v, dur)
  | other -> other

let transP = transposePitch

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

(*
let mapResult4 f a b c d =
  match (a, b, c, d) with
  | Ok a', Ok b', Ok c', Ok d' -> Ok (f a' b' c' d')
  | Error a', _, _, _ -> Error a'
  | _, Error b', _, _ -> Error b'
  | _, _, Error c', _ -> Error c'
  | _, _, _, Error d' -> Error d'
*)

let applyResult fRes res =
  (* applicative =  m (a -> b) -> m a -> m b *)
  let ( >>= ) ma f = Result.bind ma f in
  fRes >>= fun fa ->
  res >>= fun r -> Result.ok (fa r)

let mapResult4 f4 a b c d =
  let ( <*> ) f ma = applyResult f ma in
  Result.ok f4 <*> a <*> b <*> c <*> d

let mkNote c p v d =
  mapResult4
    (fun ch pi ve dt -> NoteEvent (ch, pi, ve, dt))
    (mkChannel c) (mkPitch p) (mkVelocity v) (mkSamps d)

let zipToNoteEvt c p v d =
  zipWith4 (fun ch pi ve du -> NoteEvent (ch, pi, ve, du)) c p v d

let mkNoteClip c p v d =
  (* always returns a note, even if your parameters are out of range
     channel pitch velocity duration
  *)
  let ch = mkChannelClip c in
  let pi = mkPitchClip p in
  let ve = mkVelocityClip v in
  let dt = mkSampsClip d in
  NoteEvent (ch, pi, ve, dt)

let mapOverPitch f evt =
  (* transform pitch: output of f is clipped to garentee valid pitch *)
  match evt with
  | NoteEvent (c, Pitch p, v, d) -> NoteEvent (c, mkPitchClip (f p), v, d)
  | other -> other

let mapOverCh f evt =
  (* transform channel. Output of f is clipped  *)
  match evt with
  | NoteEvent (MidiCh c, p, v, d) -> NoteEvent (mkChannelClip (f c), p, v, d)
  | other -> other

let mapOverVelo f evt =
  (* output of f is clipped to garentee valid pitch *)
  match evt with
  | NoteEvent (c, p, Velo v, d) -> NoteEvent (c, p, mkVelocityClip (f v), d)
  | other -> other

let mapOverDuration f evt =
  match evt with
  | NoteEvent (c, p, v, Samps d) -> NoteEvent (c, p, v, Samps (f d))
  | other -> other

(* applicative
   (a -> b -> c) -> Seq.t a -> Seq.t b -> Seq.t c
*)

let withChannel c evt =
  match evt with NoteEvent (_, p, v, d) -> NoteEvent (c, p, v, d) | any -> any

let withPitch p evt =
  match evt with NoteEvent (c, _, v, d) -> NoteEvent (c, p, v, d) | any -> any

let withVelo v evt =
  match evt with NoteEvent (c, p, _, d) -> NoteEvent (c, p, v, d) | any -> any

let withDur d evt =
  match evt with NoteEvent (c, p, v, _) -> NoteEvent (c, p, v, d) | any -> any

let withPitchSq = map2 withPitch
let withDurSq = map2 withDur
let withChannelSq = map2 withChannel
let withVeloSq = map2 withVelo

type pitchBend = Pitchbend of int (* -8192 to 8191 *)

let pb_to_int (Pitchbend pb) = pb
let getFirstSevenBits v = v land 0b01111111
let getSecondSevenBits v = (v lsr 7) land 0b01111111

let pitchBendToRaw (Pitchbend pb) =
  let shift = pb + 8192 in
  let lsb = getFirstSevenBits shift in
  let msb = getSecondSevenBits shift in
  (0xe0, lsb, msb)

let pitchBendFromRaw (lsb, msb) =
  let shift = (msb lsl 7) lor lsb in
  Pitchbend (shift - 8192)

type midiMessage =
  | NoteOn of midiChannel * pitch * velocity
  | NoteOff of midiChannel * pitch * velocity
  | Control of midiChannel * controller * midiValue
  | PitchBend of midiChannel * pitchBend
  | PolyAftertouch of midiChannel * pitch * midiValue
  | ClockTick
  | ClockStart
  | ClockStop
  | MidiSilence

(* open questino: should midiMessage be used with midiMessage option, so to avoid MidiSilence ? *)

type noteMsg =
  | On of midiChannel * pitch * velocity
  | Off of midiChannel * pitch * velocity

let onlyNotes midiMessageSq =
  let f msg =
    match msg with
    | NoteOn (ch, pi, ve) -> Some (On (ch, pi, ve))
    | NoteOff (ch, pi, ve) -> Some (Off (ch, pi, ve))
    | _ -> None
  in
  Seq.map f midiMessageSq

let mapOverMidiPitch f msg =
  match msg with
  | NoteOn (ch, Pitch p, v) -> NoteOn (ch, Pitch (f p |> clip 0 127), v)
  | NoteOff (ch, Pitch p, v) -> NoteOff (ch, Pitch (f p |> clip 0 127), v)
  | other -> other

let mapPitch f msg =
  (* function that maps over the pitch of an event *)
  match msg with
  | NoteOn (_, Pitch p, _) -> Some (f p)
  | NoteOff (_, Pitch p, _) -> Some (f p)
  | _ -> None

let getPitch msg =
  match msg with
  | NoteOn (_, Pitch p, _) -> Some p
  | NoteOff (_, Pitch p, _) -> Some p
  | _ -> None

let mapOverNotes f midiMsgs ctrlSq =
  recursive midiMsgs (MidiSilence, ctrlSq)
    (fun input state ->
      match (input, (snd state) ()) with
      | NoteOn (_, _, _), Cons (ctrl, ctrlTail) -> (f input ctrl, ctrlTail)
      | otherMsg, state -> (otherMsg, fun () -> state))
    (fun state -> fst state)

let mapOverEvents f midiEvts ctrlSq =
  recursive midiEvts (SilenceEvent, ctrlSq)
    (fun input state ->
      match (input, (snd state) ()) with
      | NoteEvent _, Cons (ctrl, ctrlTail) -> (f input ctrl, ctrlTail)
      | otherEvent, state -> (otherEvent, fun () -> state))
    (fun state -> fst state)

let mapSeqOverPitch f inputMidiMsg pitchSq =
  let overPitch msg ctrl =
    match msg with
    | NoteOn (MidiCh ch, Pitch p, Velo v) ->
        NoteOn (MidiCh ch, mkPitchClip (f p ctrl), Velo v)
    | otherMsg -> otherMsg
  in
  mapOverNotes overPitch inputMidiMsg pitchSq

let hasHigherPitch msga msgb =
  getPitch msga |> Option.map ( > ) |> optionAndMap (getPitch msgb)

let isNoteOn midiMsg =
  match midiMsg with NoteOn (_, _, _) -> true | _ -> false

module ControllerMap = Map.Make (Int)
module KeyPitchMap = Map.Make (Int)

module MidiState = struct
  type t = {
    depressedNotes : velocity KeyPitchMap.t;
    controlValues : midiValue ControllerMap.t;
    currentNote : (midiChannel * pitch * velocity) option;
    currentPitch : pitch;
  }

  (* these types are for safety, so we do not mix up things *)
  type pitchMapKey = PitchMapKey of int
  type ctrlMapKey = CtrlMapKey of int

  (* this is the note that was played just now, for triggering, default is none *)

  let chanPitchToKey (MidiCh ch) (Pitch p) = PitchMapKey (p + (ch * 128))
  let keyToChanPitch (PitchMapKey key) = (MidiCh (key / 128), key mod 128)
  let chanCtrlToKey (MidiCh ch) (MidiCtrl c) = CtrlMapKey (c + (ch * 128))
  let keyToChanCtrl (CtrlMapKey key) = (key / 128, key mod 128)
  let justTheKey (PitchMapKey mapKey) = Pitch (mapKey mod 128)

  let empty =
    {
      depressedNotes = KeyPitchMap.empty;
      controlValues = ControllerMap.empty;
      currentNote = None;
      currentPitch = Pitch 60;
    }

  (** map.add = x y m = item key map *)
  let update midiMsg state =
    match midiMsg with
    | NoteOn (midiCh, pitch, v) ->
        let (PitchMapKey key) = chanPitchToKey midiCh pitch in
        {
          state with
          depressedNotes = KeyPitchMap.add key v state.depressedNotes;
          currentNote = Some (midiCh, pitch, v);
          currentPitch = pitch;
        }
    | NoteOff (midiCh, pitch, Velo _) ->
        let (PitchMapKey key) = chanPitchToKey midiCh pitch in
        {
          state with
          depressedNotes = KeyPitchMap.remove key state.depressedNotes;
          currentNote = None;
        }
    | Control (midiCh, ctrl, value) ->
        let (CtrlMapKey key) = chanCtrlToKey midiCh ctrl in
        {
          state with
          controlValues = ControllerMap.add key value state.controlValues;
          currentNote = None;
        }
    | _ -> { state with currentNote = None }

  let makeSeq midiMsgSq = recursive midiMsgSq empty update id
  let makeTrigger stateSq = Seq.map (fun state -> state.currentNote) stateSq

  let getVeloOfKey key keyPitchMap =
    let mvalue = KeyPitchMap.find_opt key keyPitchMap in
    match mvalue with None -> Velo 0 | Some v -> Velo v

  let filterChannel (MidiCh channel) keyPitchMap =
    KeyPitchMap.filter (fun key _ -> key / 128 == channel) keyPitchMap

  let getPitchMapKeys state =
    state.depressedNotes |> KeyPitchMap.bindings
    |> List.map (mapFst (fun key -> PitchMapKey key))

  let getControlMapKeys state =
    state.controlValues |> ControllerMap.bindings
    |> List.map (mapFst (fun key -> CtrlMapKey key))

  let getDepressedKeysAnyChannel state =
    state |> getPitchMapKeys |> List.map (mapFst justTheKey)

  let getDepressedKeysChannel channel state =
    state.depressedNotes |> filterChannel channel |> KeyPitchMap.bindings
    |> List.map (mapFst (fun k -> PitchMapKey k |> justTheKey))

  let getFirstNote channel state =
    let lst = getDepressedKeysChannel channel state in
    match lst with [] -> (Pitch 0, Velo 0) | (p, v) :: _ -> (p, v)

  (* zero if there is no controller *)
  let getControllerValue channel ctrlNumber state =
    let (CtrlMapKey key) = chanCtrlToKey channel ctrlNumber in
    state.controlValues |> ControllerMap.find_opt key
    |> Option.value ~default:(MidiVal 0)

  let getControlR midiCh ctrlNumber =
    let open Reader.Ops in
    Reader.ask () >>= fun env ->
    getControllerValue midiCh ctrlNumber env |> Reader.return

  let getCurrentNote =
    let open Reader.Ops in
    Reader.ask () >>= fun state -> state.currentNote |> Reader.return

  let triggerFromCurrent duration state =
    match state.currentNote with
    | Some (MidiCh ch, Pitch p, Velo v) -> mkNoteClip ch p v duration
    | None -> SilenceEvent

  let triggerR duration =
    let open Reader.Ops in
    Reader.ask () >>= fun env ->
    triggerFromCurrent duration env |> Reader.return

  let getDepressedR channel =
    let open Reader.Ops in
    Reader.ask () >>= fun env ->
    getDepressedKeysChannel channel env |> Reader.return

  let triggerOptionR value =
    let open Reader.Ops in
    Reader.ask () >>= fun state ->
    let opt = match state.currentNote with Some _ -> Some value | _ -> None in
    Reader.return opt

  let triggerFromNoteR f =
    let open Reader.Ops in
    Reader.ask () >>= fun state ->
    let opt =
      match state.currentNote with
      | Some (c, p, v) -> Some (f (Note (c, p, v)))
      | None -> None
    in
    Reader.return opt

  let boolFromNote =
    let open Reader.Ops in
    Reader.ask () >>= fun state ->
    Reader.return (Option.is_some state.currentNote)

  let getPitchR =
    let open Reader.Ops in
    Reader.ask () >>= fun state -> Reader.return state.currentPitch

  let boolFromChannelR channel =
    let open Reader.Ops in
    Reader.ask () >>= fun state ->
    let bool =
      match state.currentNote with
      | Some (c, _, _) -> isChannel c channel
      | _ -> false
    in
    Reader.return bool

  let getCurrentOfChannel channel state =
    let curr = state.currentNote in
    match curr with
    | Some (ch, _, _) -> if ch == channel then curr else None
    | None -> None
end

(*

   convert everything into a stream of records, as options

   so Some r, Some r, None, None, None, Some r

   then have something map that back to midi stream
*)

let prepend prefix str = prefix ^ str

let midiToString = function
  | NoteOn (MidiCh ch, Pitch p, Velo v) ->
      List.map Int.to_string [ ch; p; v ]
      |> String.concat "-" |> prepend "NoteOn "
  | NoteOff (MidiCh ch, Pitch p, Velo v) ->
      List.map Int.to_string [ ch; p; v ]
      |> String.concat "-" |> prepend "NoteOff "
  | PitchBend (MidiCh ch, pb) ->
      Int.to_string ch ^ "pitch bend" ^ " " ^ Int.to_string (pb_to_int pb)
  | PolyAftertouch (MidiCh ch, Pitch p, MidiVal v) ->
      Int.to_string ch ^ "poly aftertouch" ^ " " ^ Int.to_string p ^ " "
      ^ "pressure" ^ " " ^ Int.to_string v
  | Control (MidiCh ch, MidiCtrl ctrl, MidiVal v) ->
      List.map Int.to_string [ ch; ctrl; v ]
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
  | PitchBend (MidiCh ch, Pitchbend pbint) ->
      (0xe0 lor ch, getFirstSevenBits pbint, getSecondSevenBits pbint)
  | PolyAftertouch (MidiCh ch, Pitch p, MidiVal v) -> (0xa0 lor ch, p, v)
  | ClockTick -> (0xf8, 0, 0)
  | ClockStart -> (0xf6, 0, 0)
  | ClockStop -> (0xfa, 0, 0)
  | MidiSilence -> (0, 0, 0)

let chFromByte byte = MidiCh (byte land 0x0f)

let fromRaw (status, data1, data2) =
  let statusByte = status land 0xf0 in
  match statusByte with
  | 0x90 -> (
      match data2 with
      (* Zero velocity note on, lets not *)
      | 0 -> NoteOff (chFromByte status, Pitch data1, Velo data2)
      | velo -> NoteOn (chFromByte status, Pitch data1, Velo velo))
  | 0x80 -> NoteOff (chFromByte status, Pitch data1, Velo data2)
  | 0xb0 -> Control (chFromByte status, MidiCtrl data1, MidiVal data2)
  | 0xf8 -> ClockTick
  | 0xf6 -> ClockStart
  | 0xfa -> ClockStop
  | _ -> MidiSilence

let fromMidiMsgWithDur defaultDuration msg =
  match msg with
  | NoteOn (ch, p, v) -> NoteEvent (ch, p, v, defaultDuration)
  | _ -> SilenceEvent

(*
let defaultTranslator  = {
    onNoteOn = fun NoteOn (_,_,_) -> SilenceEvent  *)

let printRaw (status, data1, data2) =
  if status != 0 then
    let () =
      print_char '\n';
      print_int status;
      print_char '-';
      print_int data1;
      print_char '-';
      print_int data2
    in
    ()
  else ()

(** intersperce a Seq with silence 
  M...M...M...M... 
  Note that the time is provided as samples
  *)
let withInterval (interval : deltaT Seq.t) (fillerEvent : 'a) (sq : 'a Seq.t) =
  let ctrl = zip sq interval in
  ctrl
  |> concatMap (fun (src, Samps n) () -> Cons (src, Cisp.repeat n fillerEvent))

let withInt interval fillerEvent sq =
  let ctrl = zip sq interval in
  concatMap (fun (src, n) () -> Cons (src, fun () -> repeat n fillerEvent)) ctrl

type ordering = Greater | Smaller | Equal

let rec insertBy cmp v sq () =
  match sq () with
  | Nil -> Cons (v, fun () -> Nil)
  | Cons (h, tl) -> (
      match cmp h v with
      | Greater -> Cons (h, insertBy cmp v tl)
      | _ -> Cons (v, sq))

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
  midiToString msg |> print_string;
  print_newline ()

let print_timed_midi_event (t, msg) =
  let () =
    print_string "t=";
    print_int t;
    print_string "|";
    print_midi_msg msg;
    print_newline ()
  in
  ()

(* this takes a stream of midi events with a start and duration, and renders it as a flat stream of midi. It tries to handle intellegentily when events happen at the same moment. See updatemidi for the details *)
type midiSerializer = {
  now : int;
  pendingNoteOffs : timedMidiEvent Fqueue.t;
  deferred : midiMessage Fqueue.t;
}

let initSerializer : midiSerializer =
  { now = 0; deferred = Fqueue.empty; pendingNoteOffs = Fqueue.empty }

let peek = Fqueue.peek
let dequeue = Fqueue.dequeue
let enqueue = Fqueue.enqueue

let enqueueOnlyNotes v q =
  match v with MidiSilence -> q | nonSilence -> Fqueue.enqueue nonSilence q

let print_state m label =
  let now = m.now in
  let deferred = Fqueue.to_list m.deferred in
  let pendingNoteOffs = Fqueue.to_list m.pendingNoteOffs in
  print_string label;
  print_newline ();
  print_string "now: ";
  print_int now;
  print_string
    (" pending offs: "
    ^ Int.to_string (List.length pendingNoteOffs)
    ^ "-" ^ "\n");
  List.iter print_timed_midi_event pendingNoteOffs;
  print_string "\n";
  print_string (" deferred: " ^ Int.to_string (List.length deferred) ^ "-\n");
  List.iter print_midi_msg deferred;
  print_newline ()

let getPending newEvt m =
  (* let () = print_state m "pending" in *)
  let pending = peek m.pendingNoteOffs in
  match pending with
  | Some (t, evt) ->
      if t <= m.now then
        ( evt,
          {
            m with
            pendingNoteOffs = dequeue m.pendingNoteOffs;
            deferred = enqueueOnlyNotes newEvt m.deferred;
          } )
      else (newEvt, m)
  | None -> (newEvt, m)

let getDeferred newEvt m =
  (* let () = print_state m "deferred" in *)
  let d = peek m.deferred in
  match d with
  | Some devt ->
      (devt, { m with deferred = enqueueOnlyNotes newEvt (dequeue m.deferred) })
  | None -> (newEvt, m)

let handleMidiEvent midiEvt m =
  match midiEvt with
  | NoteEvent (ch, p, v, Samps dura) ->
      let noteOff = (m.now + dura, NoteOff (ch, p, v)) in
      ( NoteOn (ch, p, v),
        { m with pendingNoteOffs = enqueue noteOff m.pendingNoteOffs } )
  | ControlEvent (ch, ctrl, v) -> (Control (ch, ctrl, v), m)
  | SilenceEvent -> (MidiSilence, m)

let nowPlusOne evt m = ({ m with now = m.now + 1 }, evt)

let mergeBundles (Bundle (midiEvtA, midiEvtsA)) (Bundle (midiEvtB, midiEvtsB)) =
  Bundle (midiEvtA, fun () -> Seq.Cons (midiEvtB, append midiEvtsA midiEvtsB))

let mergeBundlesSq bundleSq1 bundleSq2 =
  zipWith mergeBundles bundleSq1 bundleSq2

type midiEventSeq = midiEvent Seq.t

let emptyBundle = Bundle (SilenceEvent, Seq.empty)

let polyphoneBundlesSq (seq : midiEvent Seq.t Seq.t) =
  let soloBundle =
    Seq.map (fun midiEvtSq -> Seq.map soloBundle midiEvtSq) seq
  in
  Seq.fold_left mergeBundlesSq (st emptyBundle) soloBundle

let polyphoneBundlesLst (seqLst : midiEvent Seq.t list) =
  let mapIntoBundle =
    List.map (fun midiEvtSq -> Seq.map soloBundle midiEvtSq) seqLst
  in
  List.fold_left mergeBundlesSq (st emptyBundle) mapIntoBundle

let handleBundle (Bundle (midiEvt, midiEvts)) m0 =
  let queueEvent m evt =
    (* acc x *)
    match evt with
    | NoteEvent (ch, p, v, Samps dura) ->
        let noteOff = (m.now + dura, NoteOff (ch, p, v)) in
        {
          m with
          pendingNoteOffs = enqueue noteOff m.pendingNoteOffs;
          deferred = enqueueOnlyNotes (NoteOn (ch, p, v)) m.deferred;
        }
    | ControlEvent (ch, ctrl, v) ->
        {
          m with
          deferred = enqueueOnlyNotes (Control (ch, ctrl, v)) m.deferred;
        }
    | SilenceEvent -> m
  in
  let currentNote, m1 = handleMidiEvent midiEvt m0 in
  let m2 = Seq.fold_left queueEvent m1 midiEvts in
  (currentNote, m2)

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

let updateMidiBundle bundle m =
  let ( ||> ) (evt, m) f = f evt m in
  let state, evt =
    (bundle, m) ||> handleBundle ||> getPending ||> getDeferred ||> nowPlusOne
  in
  (evt, state)

let serializeBundles bundles =
  let startM = initSerializer in
  let rec aux msg_sq model () =
    match msg_sq () with
    | Nil -> Nil
    | Cons (evt, tl) ->
        let e, state = updateMidiBundle evt model in
        (* segmentation fault !!
           let () = print_state state "\n\n\n" in *)
        Cons (e, aux tl state)
  in
  aux bundles startM

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
  let amp = ch [| 100; 90; 100 |] in
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


(** deprecated, these could be replaced by Seq.map2 withPitch ? *)
let rec overwritePitch pitchSq evtSeq () =
  match evtSeq () with
  | Cons (NoteEvent (c, _, v, d), tl) -> (
      match pitchSq () with
      | Cons (p, ptail) ->
          let pitch = mkPitchClip p in
          Cons (NoteEvent (c, pitch, v, d), overwritePitch ptail tl)
      | Nil -> Nil)
  | Cons (otherEvent, tl) -> Cons (otherEvent, overwritePitch pitchSq tl)
  | Nil -> Nil

let rec overwriteDur durSq sq () =
  match sq () with
  | Cons (NoteEvent (c, p, v, _), tl) -> (
      match durSq () with
      | Cons (dur, dtl) ->
          let d = mkSampsClip dur in
          Cons (NoteEvent (c, p, v, d), overwriteDur dtl tl)
      | Nil -> Nil)
  | Cons (event, tl) -> Cons (event, overwriteDur durSq tl)
  | Nil -> Nil

let rec overwriteChan chanSq sq () =
  match sq () with
  | Cons (NoteEvent (_, p, v, d), tl) -> (
      match chanSq () with
      | Cons (chan, chtl) ->
          let c = mkChannelClip chan in
          Cons (NoteEvent (c, p, v, d), overwriteChan chtl tl)
      | Nil -> Nil)
  | Cons (event, tl) -> Cons (event, overwriteChan chanSq tl)
  | Nil -> Nil

let rec overwriteVelo veloSq sq () =
  match sq () with
  | Cons (NoteEvent (c, p, _, d), tl) -> (
      match veloSq () with
      | Cons (velo, vtl) ->
          let v = mkVelocityClip velo in
          Cons (NoteEvent (c, p, v, d), overwriteVelo vtl tl)
      | Nil -> Nil)
  | Cons (event, tl) -> Cons (event, overwriteVelo veloSq tl)
  | Nil -> Nil

let rec difference sq start () =
  match sq () with
  | Nil -> Nil
  | Cons (h, tl) -> Cons (h - start, difference tl h)

let onlyPitch sq = map fst sq
let onlyVelo sq = map snd sq
let seconds s = Samps (44100.0 *. s |> Int.of_float)
let timing = seconds 0.01 |> st

let testSequence =
  withInterval (Cisp.st (Samps 4)) MidiSilence
    (map (fun i -> NoteOn (MidiCh 1, Pitch (60 + (i mod 12)), Velo 100)) count)

let justSilence = ref (st (toRaw MidiSilence))
let makeNote pitch velo dur channel = (pitch, velo, dur, channel)

  (** pitch, velocity, duration, channel*)
let makeNoteOfInts p v d ch =

  NoteEvent (mkChannelClip ch, mkPitchClip p, mkVelocityClip v, mkSampsClip d)

let filterEvents f sq =
  map (fun midiEvent -> if f midiEvent then midiEvent else SilenceEvent) sq

let hasChannel ch event =
  match event with
  | NoteEvent (MidiCh c, _, _, _) -> ch == c
  | ControlEvent (MidiCh c, _, _) -> ch == c
  | SilenceEvent -> false

let filterCh ch sq = filterEvents (hasChannel ch) sq
let c3 = mkNoteClip 1 60 100 1000
let c4 = mkNoteClip 1 72 100 1000

let skip n sqIn =
  let rec aux m sq =
    match sq () with
    | Nil -> Nil
    | Cons (evt, tail) -> (
        match evt with
        | NoteEvent (MidiCh c, Pitch p, Velo v, Samps d) ->
            if m = 0 then
              Cons
                ( NoteEvent (MidiCh c, Pitch p, Velo v, Samps d),
                  fun () -> aux m tail )
            else Cons (SilenceEvent, fun () -> aux (m - 1) tail)
        | other -> Cons (other, fun () -> aux m tail))
  in
  aux n sqIn

type delayedNote = DelayedNote of deltaT * midiEvent

let printDelNote (DelayedNote (Samps dt, mEvt)) =
  print_int dt;
  printMidiEvent mEvt

let getDelayedNote (DelayedNote (_, evt)) = evt

let shiftDelayedNote offset (DelayedNote (deltaT, evt)) =
  DelayedNote (offsetSamps offset deltaT, evt)

let mkDelayedNote samps note =
  let absolute = if samps < 0 then samps * -1 else samps in
  DelayedNote (Samps absolute, note)

let mkDelNote = mkDelayedNote
let compareDelNote (DelayedNote (ta, _)) (DelayedNote (tb, _)) = ta < tb
let getTimeOfDelNote (DelayedNote (ta, _)) = ta
let isDelNoteNow (DelayedNote (ta, _)) tb = compareSamps ta tb
let isDelNotePassed (DelayedNote (ta, _)) tNow = not (compareSamps ta tNow)

(* garantee that notes are sorted *)
type midiScore = MidiScore of delayedNote sorted

let printMidiScore (MidiScore (Sorted dNotes)) =
  List.iter (fun nt -> printDelNote nt) dNotes

let emptyScore = MidiScore emptySorted

let insertNoteInScore (MidiScore (Sorted lst)) note =
  MidiScore (Sorted (insertSorted compareDelNote lst note))

let unconsDelNotes (MidiScore (Sorted lst)) =
  match lst with h :: tl -> Some (h, MidiScore (Sorted tl)) | [] -> None

let scoreOfList lst = List.fold_left insertNoteInScore emptyScore lst
let scoreOfSeq sq = Seq.fold_left insertNoteInScore emptyScore sq
let ofScore (MidiScore (Sorted lst)) = lst

(** 
val arpeggiator : arpeggio Seq.t -> midiBundle Seq.t
 *)

let mergeScores scoreA scoreB =
  let notesA = ofScore scoreA in
  List.fold_left insertNoteInScore scoreB notesA

let shiftScoreTime (offset : deltaT) (MidiScore dNotes) =
  let f = Linear (shiftDelayedNote offset) in
  MidiScore (mapSortedLinear f dNotes)

type midiPlayer = PlayState of { now : deltaT; score : midiScore }

let printMidiPlayer (PlayState { now; score }) =
  print_string "playerState\n";
  match (now, score) with
  | Samps s, scr ->
      print_string ("now: " ^ string_of_int s ^ "\n");
      printMidiScore scr

let updateMidiPlayerTime (PlayState player) =
  PlayState { player with now = oneTick player.now }

let cleanup (PlayState { now; score }) =
  match score with
  | MidiScore (Sorted lst) ->
      PlayState
        {
          now;
          score =
            MidiScore
              (Sorted (List.filter (fun n -> isDelNotePassed n now) lst));
        }

let addScoreToPlayer (newScore : midiScore) (PlayState { now; score }) =
  let scoreWithOffset = shiftScoreTime now newScore in
  PlayState { now; score = mergeScores score scoreWithOffset }

let init scoreSq =
  match scoreSq () with
  | Cons (Some scr, tl) -> (PlayState { now = Samps 0; score = scr }, tl)
  | Cons (None, tl) -> (PlayState { now = Samps 0; score = emptyScore }, tl)
  | Nil -> (PlayState { now = Samps 0; score = emptyScore }, fun () -> Nil)

let playArp scoreSq =
  (* how to time relatively *)
  let s0, scoreTail = init scoreSq in
  let update evt player =
    let s1 =
      match evt with
      | None -> player
      | Some score -> addScoreToPlayer score player
    in
    s1 |> cleanup |> updateMidiPlayerTime
  in
  let eval (PlayState { score; now }) =
    let notes = ofScore score in
    let currentNotes =
      List.filter (fun delNote -> isDelNoteNow delNote now) notes
    in
    match currentNotes with
    | [] -> silenceBundle
    | h :: ts ->
        Bundle (getDelayedNote h, List.to_seq (List.map getDelayedNote ts))
  in
  recursive scoreTail s0 update eval

type relNote = RelNote of deltaT * midiEvent

let relToScore relScore =
  let seqRev (clock, lst) note =
    let wait, evt = match note with RelNote (Samps w, e) -> (w, e) in
    let newClock = clock + wait in
    (newClock, DelayedNote (Samps newClock, evt) :: lst)
  in
  let notes = List.fold_left seqRev (0, []) relScore |> snd in
  MidiScore (Sorted (List.rev notes))

let mergeMidiStreams sqa sqb =
  (* this merges streams of midi events, so it becomes one stream of events. When two events "collide", it pushes one onto the stack.
     events on the stack have priority on newer events
  *)
  let module Q = Fqueue in
  let handleEvents (a, b) (overflow, _) =
    let optFlow = Q.peekdeq overflow in
    match (a, b, optFlow) with
    | SilenceEvent, SilenceEvent, None -> (Q.empty, SilenceEvent)
    | SilenceEvent, SilenceEvent, Some (evt, rest) -> (rest, evt)
    | SilenceEvent, evtb, Some (evt, rest) -> (Q.enqueue evtb rest, evt)
    | evta, SilenceEvent, Some (evt, rest) -> (Q.enqueue evta rest, evt)
    | evta, SilenceEvent, None -> (Q.empty, evta)
    | SilenceEvent, evtb, None -> (Q.empty, evtb)
    | evta, evtb, None -> (Q.enqueue evtb Q.empty, evta)
    | evta, evtb, Some (evt, rest) ->
        (rest |> Q.enqueue evta |> Q.enqueue evtb, evt)
  in
  let zipped = Cisp.zip sqa sqb in
  recursive zipped (Q.empty, SilenceEvent) handleEvents (fun (_, evt) -> evt)

let mkRhythm sq note rest =
  (* note and rest should be of type int Seq.t 
   * this function will return note x sqs then rest x silenceevents and repeat 
   * mkRhythm (seq [1;2;3] (st 3) (st 2)
   * note 1 2 _ _ 3 1 2 _ _ *)
  seq [ sq; SilenceEvent ] |> hold (interleave note rest)

(* previous implementatino that caused CPU creep!!!
   let filler =
     (st SilenceEvent)
   in
   [ group note sq ; group rest filler ] |> ofList |> transcat |> concat
*)

(* TODO make function that weaves any number of streams together using an index *)
(* Ideally the thing is a list *)

(*
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
 *)

let overTrigger events trigger = weavePattern trigger events (st SilenceEvent)

(**
  * note this function was updated
  * @event is the note events you want to generate
  * @midiIn is the sync input, so any note on, will trigger midiEvents
  *)
let trigger event midiIn =
  let t = map isNoteOn midiIn in
  Cisp.weavePattern t event (st SilenceEvent)

let testmidi midi = midi |> take 80 |> serialize |> iter (toRaw >> printRaw)

let playMidi midiSq samplerateRef =
  (* midiSq is a function that takes an input seq as argument and retuns a raw midi seuence.
     * You also pass in a samplerateRef, which can be used for samplerate dependent calculations.
  *)
  let state = justSilence in
  (* the sq state var *)
  let inputRef = ref MidiSilence in
  let () = state := ofRef inputRef |> midiSq in
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
  JackMidi.playMidi callback samplerateRef

type midiNoteGenerator =
  | MidiNoteGen of {
      pitch : int Seq.t;
      velo : int Seq.t;
      durInSec : float Seq.t;
      channel : int Seq.t;
    }

let genWithPitch sq (MidiNoteGen gen) =
  MidiNoteGen { gen with pitch = Cisp.intify sq }

let genWithVelo sq (MidiNoteGen gen) =
  MidiNoteGen { gen with velo = Cisp.intify sq }

let genWithDur sq (MidiNoteGen gen) = MidiNoteGen { gen with durInSec = sq }

let genWithChannel sq (MidiNoteGen gen) =
  MidiNoteGen { gen with channel = Cisp.intify sq }

let sec_to_samps s = s |> ( *. ) !Process.sample_rate |> int_of_float

(** could also use infseq, but for now use finseq
   instead of using Seq.t, where the input is (),
     we are explicitely requiring the user to provide the previous midi generator state,
     so it can be changed by the user "on-the-fly"
  *)
let play_note (MidiNoteGen { pitch; velo; durInSec; channel }) =
  let ( >>= ) a f = Option.bind a f in
  uncons pitch >>= fun (p, p_tail) ->
  uncons velo >>= fun (v, v_tail) ->
  uncons durInSec |> Option.map (mapFst sec_to_samps) >>= fun (d, d_tail) ->
  uncons channel >>= fun (c, c_tail) ->
  Option.Some
    ( makeNoteOfInts p v d c,
      MidiNoteGen
        { pitch = p_tail; velo = v_tail; durInSec = d_tail; channel = c_tail }
    )

let fromGenerator (MidiNoteGen { pitch; velo; durInSec; channel }) input =
  let durInSamp =
    map
      (function s -> s |> ( *. ) !Process.sample_rate |> int_of_float)
      durInSec
  in
  let stream = makeNoteOfInts <$> pitch <*> velo <*> durInSamp <*> channel in
  input |> trigger stream |> serialize |> map toRaw

let from_dynamic_generator generator custom_update input =
  let rec aux midi_generator midi_input () =
    match midi_input () with
    | Nil -> Nil
    | Cons (inp, tl) ->
        if isNoteOn inp then
          let opt = play_note midi_generator in
          match opt with
          | None -> Nil
          | Some (note, state) ->
              let new_state = custom_update state in
              Cons (note, aux new_state tl)
        else Cons (SilenceEvent, aux (custom_update midi_generator) tl)
  in
  aux generator input |> serialize |> map toRaw

(* todo make all of this use infseq *)
let from_dynamic_generators (generators : midiNoteGenerator Option.t Array.t)
    custom_update input =
  let rec aux midi_generators midi_input () =
    match midi_input () with
    | Nil -> Nil
    | Cons (inp, tl) ->
        if isNoteOn inp then
          let notes_generators =
            (* playnote produces (note, generator) Option.t *)
            Array.map (fun opt -> Option.bind opt play_note) midi_generators
          in
          let notes =
            Array.map (fun opt -> opt |> Option.map Cisp.fst) notes_generators
          in
          let states =
            Array.map (fun opt -> opt |> Option.map Cisp.snd) notes_generators
          in
          Cons
            ( Array.fold_left (Cisp.flip addOptToBundle) emptyBundle notes,
              aux states tl )
        else Cons (emptyBundle, aux (custom_update midi_generators) tl)
  in
  aux generators input |> serializeBundles |> map toRaw

