type env = Normal of {fade_in: float; fade_out: float} | DefaultEnv

type sound = Simple_sine of float | Roland of {index: int; transpose: float}

type position = {x: float; y: float}

type event =
  { start: float
  ; duration: float
  ; envelope: env
  ; sound: sound
  ; position: position }

type score = Score of {events: event list; title: string}

let roland idx transpose = Roland {index= idx; transpose}

let wfsPos x y = {x; y}

let rolandEvent start duration envelope index transpose x y =
  { start ; duration ; envelope ; sound = Roland { index; transpose }; position = wfsPos x y }

let defaultEnv = DefaultEnv 

let rolandPath =
  "/Users/casperschipper/devel/ocaml/cisp/examples/csound/concatJV1010.wav"

let rolandLength = 5732610


let quote str = "\"" ^ str ^ "\""

(* supercollider is very picky about floats *)
let print_full_precision_float f =
  (* Use "%.17g" for maximum precision, "%.1f" to ensure .0 is shown *)
  if f = 0.0 then Printf.sprintf "0.0" else Printf.sprintf "%.17g " f

let position_to_point_str {x; y} =
  "Point("
  ^ print_full_precision_float x
  ^ ","
  ^ print_full_precision_float y
  ^ ")"

let event_to_string {start; duration; envelope; sound; position} =
  match sound with
  | Simple_sine freq ->
      let fstr = freq |> print_full_precision_float in
      let starts = print_full_precision_float start in
      let durstr = print_full_precision_float duration in
      let _ = envelope in
      "UChain(" ^ starts ^ ", 0.0, " ^ durstr ^ ", [ 'sine', [ 'freq', " ^ fstr
      ^ ", 'phase', -0.25132741228718, 'amp', 0.2 ] ], [ 'wfsSource', [ \
         'point', "
      ^ position_to_point_str position
      ^ " ] ]))"
  | Roland {index; transpose} ->
      let sampStart = index * 44100 |> string_of_int in
      let sampEnd = (index + 1) * 44100 |> string_of_int in
      let starts = print_full_precision_float start in
      let durstr = print_full_precision_float duration in
      let trans = print_full_precision_float transpose in
      "UChain(" ^ starts ^ ", 0.0," ^ durstr
      ^ ", [ 'bufSoundFile', [ 'soundFile', BufSndFile.newBasic("
      ^ quote rolandPath ^ ", " ^ string_of_int rolandLength ^ ", 1, 44100,"
      ^ sampStart ^ "," ^ sampEnd ^ "," ^ trans
      ^ ", false).hasGlobal_(true) ] ], [ 'wfsSource', [ 'point', "
      ^ position_to_point_str position
      ^ " ] ])"

let score title events = Score {title; events}

let score_to_string (Score {title; events}) =
  let evt_strs = List.map event_to_string events |> String.concat "," in
  "UScore(" ^ evt_strs ^ ").name_(" ^ quote title ^ ")"

(*** test *)

let test_sound = roland 10 1.0

let test_events =
  [ { start= 0.0
    ; duration= 1.0
    ; envelope= Normal {fade_in= 0.0; fade_out= 0.0}
    ; sound= test_sound
    ; position= wfsPos 0.0 0.0 }
  ; { start= 1.0
    ; duration= 1.0
    ; envelope= Normal {fade_in= 0.0; fade_out= 0.0}
    ; sound= test_sound
    ; position= wfsPos 10.0 0.0 } ]

let test () = print_string (score_to_string (score "fish" test_events))
