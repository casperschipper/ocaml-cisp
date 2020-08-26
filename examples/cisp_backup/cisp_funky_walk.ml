open Seq

(* Seq is a thunk that when forced returns a value and a thunk to get the tail *)

(* 
this is the same as 
let thunk x = fun () -> x 
 *)

let csr = 44100

let csrf = 44100.

(* let midi_input = ref (Array.init 1024 (fun _ -> (0, 0, 0)))*)
let midi_input = ref (0, 0, 0)

let thunk x () = x

let emptySt () = Nil

let force l = l ()

let ( |> ) x f = f x

let ( <| ) f x = f x

let ( >> ) f g x = g (f x)

let ( << ) f g x = f (g x)

let fst (x, _) = x

let snd (_, x) = x

let flip f a b = f b a

let ofInt = Float.of_int

let ofFloat = Int.of_float

let rec length sq = match sq () with Nil -> 0 | Cons (_, tl) -> 1 + length tl

let fromBinary b =
  let rec aux b base =
    if b = 0 then 0 else (b mod 10 * base) + aux (b / 10) (base * 2)
  in
  aux b 1

let rec append a b () =
  match a () with Nil -> b () | Cons (h, ls) -> Cons (h, append ls b)

(*
let rec appendSeq a b =
  match a () with
  | Nil -> b
  | Cons (this_a, rest) -> fun () -> Cons (this_a, appendSeq rest b)

let rec appendAlt a b () =
  match a () with
  | Nil -> b ()
  | Cons (h, tl) -> fun () -> Cons(h, appendAlt tl b)
*)

let rec cycle a () =
  let rec cycle_append current_a () =
    match current_a () with
    | Nil -> cycle a ()
    | Cons (this_a, rest) -> Cons (this_a, cycle_append rest)
  in
  cycle_append a ()

let rec range a b () = if a >= b then Nil else Cons (a, range (a +. 1.0) b)

(* not sure about these *)
let head ll = match ll with Nil -> None | Cons (h, _) -> Some h

let tail ll = match ll with Nil -> None | Cons (_, tl) -> Some (tl ())

let rec nth ll n =
  if n < 0 then None
  else if n = 0 then head ll
  else Option.bind (tail ll) (fun staart -> nth staart (n - 1))

(* this is not the same as ofList, but should be! 
let from_list list () =
  List.fold_right (fun x acc -> Cons (x, acc |> thunk)) list Nil
  *)

let ofList l =
  let rec aux l () = match l with [] -> Nil | x :: l' -> Cons (x, aux l') in
  aux l

let rec toList ls =
  match ls () with Nil -> [] | Cons (h, ts) -> h :: toList ts

let rec take n lst () =
  if n <= 0 then Nil
  else match lst () with Nil -> Nil | Cons (h, ts) -> Cons (h, take (n - 1) ts)

let for_example lst = lst |> take 40 |> toList

let rec drop n lst () =
  if n <= 0 then lst ()
  else match lst () with Nil -> Nil | Cons (_, tail) -> drop (n - 1) tail ()

let reverse lst =
  let rec aux acc arg () =
    match arg () with
    | Nil -> acc
    | Cons (h, ts) -> aux (Cons (h, thunk acc)) ts ()
  in
  aux Nil lst

let rec split n lst =
  if n <= 0 then (thunk Nil, lst)
  else
    match lst () with
    | Nil -> (thunk Nil, thunk Nil)
    | Cons (x, xs) ->
        let f, l = split (n - 1) xs in
        (thunk (Cons (x, f)), l)

let rec filter f lst =
  match lst () with
  | Nil -> Nil
  | Cons (h, tl) -> if f h then Cons (h, fun () -> filter f tl) else filter f tl

let rec foldr f z ll =
  match ll () with Nil -> z | Cons (h, tl) -> f h (foldr f z tl)

let rec fold_right f s acc =
  match s () with Nil -> acc | Cons (e, s') -> f e (fold_right f s' acc)

let rec concat str () =
  match str () with
  | Nil -> Nil
  | Cons (h, ls) -> (
    match h () with
    | Cons (h', ls') ->
        let newtail () = Cons (ls', ls) in
        Cons (h', concat newtail)
    | Nil -> concat ls () )

let hd lst =
  match lst () with
  | Nil -> raise (Invalid_argument "empty list has no head")
  | Cons (h, _) -> h

let tl lst =
  match lst () with
  | Nil -> raise (Invalid_argument "empty list has no tail")
  | Cons (_, tl) -> tl

let for_all f sq =
  let rec aux sq start =
    match sq () with
    | Nil -> true
    | Cons (h, tl) -> if f h then aux tl start else false
  in
  aux sq true

let is_empty sq = match sq () with Nil -> true | _ -> false

let has_more sq = is_empty sq |> not

let list_is_empty lst = match lst with [] -> true | _ -> false

let list_has_more lst = list_is_empty lst |> not

let foldHeads x acc =
  match x () with Nil -> acc | Cons (h, _) -> Cons (h, fun () -> acc)

let headsOfStreams sq () = fold_right foldHeads sq Nil

let foldTails x acc =
  match x () with Nil -> acc | Cons (_, ts) -> Cons (ts, fun () -> acc)

let tailsOfStreams sq () = fold_right foldTails sq Nil

let rec transpose sq () =
  match sq () with
  | Nil -> Nil
  | Cons (sqs, sqss) -> (
    match sqs () with
    | Cons (x, xs) ->
        Cons
          ( (fun () -> Cons (x, headsOfStreams sqss))
          , transpose <| thunk (Cons (xs, tailsOfStreams sqss)) )
    | Nil -> transpose sqss () )

let rec transpose_list lst =
  let foldHeads acc x = match x with [] -> acc | h :: _ -> h :: acc in
  let foldTails acc x = match x with [] -> acc | _ :: ts -> ts :: acc in
  match lst with
  | [] -> []
  | [] :: xss -> transpose_list xss
  | (x :: xs) :: xss ->
      (x :: List.fold_left foldHeads [] xss)
      :: transpose_list (xs :: List.fold_left foldTails [] xss)

let rec st a () =
  (* static *)
  Cons (a, st a)

let rec countFrom n () = Cons (n, countFrom (n + 1))

let count = countFrom 0

let rec zip a b () =
  match a () with
  | Nil -> Nil
  | Cons (a, atl) -> (
    match b () with Nil -> Nil | Cons (b, btl) -> Cons ((a, b), zip atl btl) )

let rec zipList a b =
  match a with
  | [] -> []
  | x :: xs -> ( match b with [] -> [] | y :: ys -> (x, y) :: zipList xs ys )

let rec zip3 a b c () =
  match (a (), b (), c ()) with
  | Cons (ha, lsa), Cons (hb, lsb), Cons (hc, lsc) ->
      Cons ((ha, hb, hc), zip3 lsa lsb lsc)
  | _ -> Nil

let rec zipWith f a b () =
  match a () with
  | Nil -> Nil
  | Cons (a, atl) -> (
    match b () with
    | Nil -> Nil
    | Cons (b, btl) -> Cons (f a b, zipWith f atl btl) )

let ( +~ ) = zipWith (fun a b -> a +. b)

let ( *~ ) = zipWith (fun a b -> a *. b)

let ( /~ ) = zipWith (fun a b -> a /. b)

let ( -~ ) = zipWith (fun a b -> a -. b)

let mixList lst () = List.fold_left ( +~ ) lst

(* zipped list will be lenght of shortest list *)

let clip low high x = if x < low then low else if x > high then high else x

let wrapf low high x =
  let range = low -. high |> abs_float in
  let modded = mod_float (x -. low) range in
  if modded < 0.0 then high +. x else low +. x

let wrap low high x =
  let range = low - high |> abs in
  let modded = (x - low) mod range in
  if modded < 0 then high + x else low + x

let rec walk start steps () =
  match steps () with
  | Cons (h, ls) ->
      let next = start +. h in
      Cons (start, walk next ls)
  | Nil -> Nil

(* operator is a function 
   to get the next value *)
let rec boundedFuncWalk start steps operator wrapfunc () =
  match steps () with
  | Cons (h, ls) ->
      let next = operator start h in
      Cons (wrapfunc start, boundedFuncWalk next ls operator wrapfunc)
  | Nil -> Nil

let boundedWalk start steps wrapfunc () =
  let rec aux start steps () =
    match steps () with
    | Nil -> Nil
    | Cons (h, ls) ->
        let next = start + h in
        Cons (wrapfunc start, aux next ls)
  in
  aux start steps

let boundedWalkf start steps wrapfunc () =
  let rec aux start steps () =
    match steps () with
    | Nil -> Nil
    | Cons (h, ls) ->
        let next = start +. h in
        Cons (wrapfunc start, aux next ls)
  in
  aux start steps

let safeIdx len idx = if idx < 0 then len - (abs idx mod len) else idx mod len

let indexArr arr idx =
  let len = Array.length arr in
  arr.(safeIdx len idx)

let rec index arr indexer () =
  match indexer () with
  | Nil -> Nil
  | Cons (idx, idxs) -> Cons (indexArr arr idx, index arr idxs)

let listWalk arr step () =
  let wrapFunc = wrap 0 (Array.length arr) in
  index arr (boundedWalk 0 step wrapFunc ())

type 'a weightList = Weights of (int * 'a) list

let mkWeights lst = match lst with [] -> None | w -> Some (Weights w)

let weights weightLst () =
  let sumWeights = List.fold_left (fun acc (_, w) -> w + acc) 0 weightLst in
  let rec lookupWeight lst curr pick =
    match lst with
    | [] -> raise <| Invalid_argument "weight not found"
    | [(value, _)] -> value
    | (value, weight) :: ws ->
        let nextCurr = weight + curr in
        if pick < nextCurr then value else lookupWeight ws nextCurr pick
  in
  let rec aux weights max () =
    Cons (lookupWeight weights 0 (Random.int max), aux weights max)
  in
  aux weightLst sumWeights ()

let rec sometimes x y p () =
  let head () =
    let rnd = Random.int p in
    if rnd < 1 then y else x
  in
  Cons (head (), sometimes x y p)

let linInterp xa xb px = ((xb -. xa) *. px) +. xa

let rec indexLin arr indexer () =
  let len = Array.length arr in
  match indexer () with
  | Nil -> Nil
  | Cons (idx, idxs) ->
      let xa = idx |> Float.floor |> Int.of_float in
      let xb = (xa + 1) mod len in
      let xp = idx -. Float.of_int xa in
      let y = linInterp (indexArr arr xa) (indexArr arr xb) xp in
      Cons (y, indexLin arr idxs)

let sineseg wavesamps =
  let incr = 1.0 /. Float.of_int wavesamps in
  let f x = 2.0 *. Float.pi *. x |> sin in
  let rec aux x () = if x >= 1.0 then Nil else Cons (f x, aux (x +. incr)) in
  aux 0.0

let arr_of_seq str = Array.of_seq str

(* TODO
let rec iterates start fs =
  match fs () with Nil -> Nil | Cons (h, ls) -> start () fs *)

let rec repeat n x () = if n > 0 then Cons (x, repeat (n - 1) x) else Nil

let hold repeats source =
  let ctrl = zip repeats source in
  map (fun (n, src) -> repeat n src) ctrl |> concat

let embed str () = Cons (str, thunk Nil) (* create a singleton stream *)

(* returnes the loops and the tail of the source
   preferably source is infinite *)
let oneLoop size n str () =
  let snippet, tail = split size str in
  (snippet |> repeat n |> concat, tail)

let loop size n src =
  let control = zip size n in
  let rec loops ctrl src () =
    match ctrl () with
    | Nil -> Nil
    | Cons ((size, num), nextCtrl) ->
        let currLoop, rest = oneLoop size num src () in
        Cons (currLoop, loops nextCtrl rest)
  in
  loops control src

let trunc = map Int.of_float

let floatify = map Float.of_int

let rvfi low high =
  let range = abs_float (low -. high) in
  let offset = min low high in
  Random.float range +. offset

let rvi low high =
  let range = abs (low - high) in
  let offset = min low high in
  Random.int range + offset

let rv low high =
  let control = zip low high in
  map (fun (l, h) -> rvi l h) control

let rvf low high =
  let control = zip low high in
  map (fun (l, h) -> rvfi l h) control

(* choice *)
let ch arr =
  let picker = rv (st 0) (st (Array.length arr)) in
  index arr picker

let mtof midi = 440.0 *. (2.0 ** ((midi -. 69.0) /. 12.0))

let ftom freq = (12.0 *. log (freq /. 440.0)) +. 69.0

let rec collatz n () =
  if n = 1 then Nil (* lets end it here *)
  else
    let even x = x mod 2 = 0 in
    let next = if even n then n / 2 else (n * 3) + 1 in
    Cons (next, collatz next)

(* use floats as arguments to somethign that expects streams *)
let lift f a b = f (st a) (st b)

let pair a b = (a, b)

let lineSegment curr target n () =
  let rate = (target -. curr) /. n in
  let reachedEnd =
    if rate > 0.0 then fun x -> x >= target else fun x -> x <= target
  in
  let rec segment curr () =
    if reachedEnd curr then Nil else Cons (curr, segment (curr +. rate))
  in
  segment curr

(* [0,1];[1,2];[2,3] etc...  *)
let rec chain start str () =
  match str () with
  | Nil -> Nil
  | Cons (h, ls) -> Cons (pair start h, chain h ls)

let selfChain str () =
  (* (a,b) (b,c) (c,d) (d,e) etc... *)
  match str () with Nil -> Nil | Cons (h, ls) -> chain h ls ()

let seq lst = lst |> ofList |> cycle

let mkLine target n () =
  let control = zip (selfChain target) n in
  map (fun ((a, b), n') -> lineSegment a b n' ()) control |> concat

let mkDel max del src () =
  let delay = Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout max in
  let index = map (fun x -> x mod max) count in
  let parameters = zip index src in
  let wr = map (fun (idx, src) -> delay.{idx} <- src) parameters in
  let readParameters = zip3 index del wr in
  map
    (fun (idx, del, wr) ->
      let maxf = ofInt max in
      let idxf = ofInt idx in
      let later = mod_float (maxf +. idxf -. del) maxf in
      let x0_idx = Int.of_float later in
      let x0 = delay.{x0_idx} in
      let x1 = delay.{(x0_idx + 1) mod max} in
      let xp = later -. Float.of_int x0_idx in
      let value = linInterp x0 x1 xp in
      wr ; value)
    readParameters

let genSine size =
  let twopi = 2.0 *. Float.pi in
  let gen i = ofInt i /. ofInt size *. twopi |> sin in
  Array.init size gen

let waveOsc arr frq =
  let arraySize = Array.length arr |> Float.of_int in
  let incr = frq /. csrf *. arraySize in
  let phasor = walk 0.0 (st incr) in
  index arr (trunc <| phasor)

let waveOscL arr frq =
  let arraySize = Array.length arr |> Float.of_int in
  let incr = frq /. csrf *. arraySize in
  let phasor = walk 0.0 (st incr) in
  indexLin arr phasor

(* some tests follow below this point *)

let threeLists = [[11; 12; 13]; [42; 43; 44]; [100; 99]]

let seqs = List.map (fun lst -> ofList lst |> cycle) threeLists

let l_seqs = ofList seqs (* make this list also a lazy stream *)

let addCount () = Cons (count, l_seqs)

(* this adds in another infinite list, wich counts natural numbers *)

let trans = transpose addCount

let cat = trans |> concat |> take 30 |> toList

let testSah = lift rvf (-0.5) 0.5 |> hold (trunc (lift rvf 1.0 1000.0))

let a = lift rvf (-1.0) 1.0

let b = lift rvf 1.0 10.0

let myLine = mkLine a b

let abcq =
  let a = List.to_seq [1; 2; 3] in
  let b = List.to_seq [20; 21; 22] in
  let c = List.to_seq [100; 102] in
  let d = List.to_seq [] in
  let ef = countFrom 999 |> take 10 in
  List.to_seq [a; b; c; d; ef]

let myLoopyLines =
  myLine ()
  |> loop (ch [|2; 4; 17; 23; 1111|]) (ch [|2; 4; 16; 32; 64; 128|])
  |> concat

(* todo *)
let holder =
  countFrom 1 |> map collatz |> concat
  |> map (fun x -> 44.1 /. Float.of_int x)
  |> map (clip 1. (Float.of_int csr))
  |> trunc

let collatzSynth = seq [-1.0; 0.5; 0.; 0.5; 1.0] |> hold holder

let fishFreqs =
  countFrom 1 |> map collatz |> concat
  |> map (fun x -> (x mod 64) + 64)
  |> floatify |> map mtof
  |> map (fun x -> Float.of_int csr /. x)
  |> trunc

let table = genSine 1024

let fish = waveOscL table 100.

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

let rec overwritePitch pitchSq sq () =
  match sq () with
  | Cons (NoteEvent (c, _, v, d), tl) -> (
    match pitchSq () with
    | Cons (pitch, ptl) ->
        let p = mkPitchClip pitch in
        Cons (NoteEvent (c, p, v, d), overwritePitch ptl tl)
    | Nil -> Nil )
  | Cons (event, tl) -> Cons (event, overwritePitch pitchSq tl)
  | Nil -> Nil

let rec overwriteDur durSq sq () =
  match sq () with
  | Cons (NoteEvent (c, p, v, _), tl) -> (
    match durSq () with
    | Cons (dur, dtl) ->
        let d = mkSampsClip dur in
        Cons (NoteEvent (c, p, v, d), overwriteDur dtl tl)
    | Nil -> Nil )
  | Cons (event, tl) -> Cons (event, overwriteDur durSq tl)
  | Nil -> Nil

let rec overwriteChan chanSq sq () =
  match sq () with
  | Cons (NoteEvent (_, p, v, d), tl) -> (
    match chanSq () with
    | Cons (chan, chtl) ->
        let c = mkChannelClip chan in
        Cons (NoteEvent (c, p, v, d), overwriteChan chtl tl)
    | Nil -> Nil )
  | Cons (event, tl) -> Cons (event, overwriteChan chanSq tl)
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

let fromMidiMsgWithDur defaultDuration msg =
  match msg with
  | NoteOn (ch, p, v) -> NoteEvent (ch, p, v, defaultDuration)
  | _ -> SilenceEvent

let printRaw (status, data1, data2) =
  if status != 0 then
    let () = print_int status ; print_int data1 ; print_int data2 in
    ()
  else ()

(* intersperce a Seq of midi-events with silence 
 * M...M...M...M...  
 * *)
let withInterval interval sq =
  let ctrl = zip sq interval in
  map (fun (src, n) () -> Cons (src, repeat n SilenceEvent)) ctrl |> concat

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
      | Cons (NoteEvent (c, p, v, d), _) ->
          Cons (NoteEvent (c, p, v, d), aux interval sq (curr - 1))
      | any -> any
  in
  aux interval sq 0

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

let print_midiMessage msg = midiToString msg |> print_string

let print_timeMidiEvent (t, msg) =
  let () =
    print_string "t=" ;
    print_int t ;
    print_string "|" ;
    print_midiMessage msg ;
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
  List.iter print_timeMidiEvent pendingNoteOffs ;
  print_string (" deferred: " ^ Int.to_string (List.length deferred) ^ "-") ;
  List.iter print_midiMessage deferred ;
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

let seconds s = 44100.0 *. s |> Int.of_float

let timing = seconds 0.01 |> st

let rec ofRef rf () = Cons (!rf, ofRef rf)

let midiInputTestFun input =
  (*let durPattern =
    transpose
      (ofList
         [ seq [10000; 1000; 1000]
         ; seq [4000; 5000; 60000; 3000]
         ; seq [1000; 10000; 500; 2000; 12000] ])
  in*)
  let pitchPattern =
    transpose
      (ofList
         [ seq [60; 64; 67; 72; 76; 79; 84]
         ; seq [53; 60; 67; 74; 81]
         ; seq (List.rev [60; 62; 64; 67; 69; 71; 72; 74; 76]) ])
    |> concat
  in
  input
  |> map (fromMidiMsgWithDur (Samps 12000))
  |> overwritePitch
       (loop (seq [3; 2; 3; 1]) (seq [2; 2; 3]) pitchPattern |> concat)
  |> overwriteDur (st 3000)
  |> overwriteChan (st 2)
  |> serialize |> map toRaw

let () =
  let state = ref (st (toRaw MidiSilence)) in
  (* the sq state var *)
  let inputRef = ref MidiSilence in
  let () = state := ofRef inputRef |> midiInputTestFun in
  let callback input =
    (* weird callback, looks like (in -> out) but reads and writes to references *)
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
  JackMidi.playMidi callback (ref 44100.0)
