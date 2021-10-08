module S = Lo.Server
module R = Result

type preset = Preset of {start_t: float; end_t: float}

type msg = Set of {voiceNr: int; preset: preset}

type data = Lo.Message.data

type state = Presets of preset Option.t Array.t

let myState = Array.make 128 None

let ofData = function
  | `Int32 n -> string_of_int n
  | `Float f | `Double f -> Printf.sprintf "%f" f
  | `String s -> s
  | `True -> "True"
  | `False -> "False"
  | _ -> "???"

type 'a oscParser =
  | OscParser of (data Seq.t -> ('a * data Seq.t, string) Result.t)

let run (OscParser p) osc = p osc

(* which is a monad *)
let return a = OscParser (fun s -> Result.ok (a, s))

(*   fmap f (Parser cs) = Parser (\s -> [(f a, b) | (a, b) <- cs s]) *)
let fmap f (OscParser p) =
  let mapFst f (a, s) = (f a, s) in
  OscParser (fun s -> p s |> Result.map (mapFst f))

let bind p f =
  let step result =
    match result with
    | R.Ok (a, rest) -> run (f a) rest
    | R.Error error -> R.Error error
  in
  OscParser
    (fun s ->
      let r1 = run p s in
      step r1 )

let ( >>= ) = bind

(* and applicative *)
let ( >> ) m k = m >>= fun _ -> k

let ( <*> ) fP xP = fP >>= fun f -> xP >>= fun x -> return (f x)

let failure err = OscParser (fun _ -> Result.Error err)

let item =
  let open Seq in
  OscParser
    (fun oscSq ->
      match oscSq () with
      | Nil -> Result.error "reached unexpected end"
      | Cons (x, xs) -> Result.Ok (x, xs) )

let satisfy predicate error =
  item
  >>= fun c ->
  print_string ("some data" ^ ofData c ^ "\n") ;
  flush stdout ;
  if predicate c then return c else failure error

let option p q =
  OscParser
    (fun s ->
      let res = run p s in
      match res with R.Error _ -> run q s | R.Ok res -> R.Ok res )

let isString str data = match data with `String s -> s = str | _ -> false

let float =
  item
  >>= fun d ->
  match d with
  | `Float f | `Double f -> return f
  | _ -> failure "expected a float"

let int =
  item
  >>= fun d ->
  match d with `Int32 i -> return i | _ -> failure "expected an int"

let flt_par parName =
  let err = "Expected parameter name: " ^ parName in
  satisfy (isString parName) err >> float

let int_par parName =
  let err = "Expected int parameter withname: " ^ parName in
  satisfy (isString parName) err >> int

let message =
  let mk voice_nr start_t end_t =
    Set {voiceNr= voice_nr; preset= Preset {start_t; end_t}}
  in
  int_par "voiceNr"
  >>= fun n ->
  flt_par "start"
  >>= fun start_t ->
  flt_par "end"
  >>= fun end_t ->
  print_float end_t ;
  print_string "endt" ;
  flush stdout ;
  let made = mk n start_t end_t in
  return made

(* OOF this is nasty, always include a unit in the function definition, otherwise it is never evaluated again *)
let update (Set {voiceNr; preset}) () =
  print_endline "write!" ;
  myState.(voiceNr) <- Some preset

let print_preset (Preset p) =
  print_string "start t:" ;
  print_float p.start_t ;
  print_string "end t:" ;
  print_float p.end_t ;
  print_newline () ;
  flush stdout

let print_state () =
  print_endline "ok" ;
  Array.iteri
    (fun idx opt ->
      match opt with
      | Some p ->
          print_string ("preset:\n" ^ string_of_int idx) ;
          print_preset p ;
          flush stdout
      | None -> () )
    myState

let testData =
  [ `String "voiceNr"
  ; `Int32 2
  ; `String "start"
  ; `Float 3.14
  ; `String "end"
  ; `Float 50.333 ]
  |> List.to_seq

let handler path data =
  let s = function
    | `Int32 n -> string_of_int n
    | `Float f | `Double f -> Printf.sprintf "%f" f
    | `String s -> s
    | `True -> "True"
    | `False -> "False"
    | _ -> "???"
  in
  let data = Array.to_list data in
  let s = List.map s data in
  let s = String.concat ", " s in
  Printf.printf "Message on %s: %s\n%!" path s

let myHandler path d =
  handler path d ;
  match path with
  | "/set" -> (
      let data = Array.to_seq d in
      let parsed = run message data in
      match parsed with
      | R.Error error ->
          print_string (error ^ "\n") ;
          flush stdout
      | R.Ok (m, _) -> update m () ; print_state () )
  | "/print" -> print_state () ; flush stdout
  | _ ->
      print_string ("unknown path: " ^ path ^ "\n") ;
      flush stdout

let bang = ref false

let midiLoop input =
  input
  |> Seq.map (fun i ->
         i |> Midi.isNoteOn |> ( := ) bang ;
         Midi.MidiSilence |> Midi.toRaw )

let midi_in () = Midi.playMidi midiLoop Process.sample_rate

let osc_receive () =
  let port = 7777 in
  let s = S.create port myHandler in
  while true do
    S.recv s
  done

module UniqueId = struct
  type t = Pool of {free: int list; used: int list}

  type id = Id of int

  let maxId = 65536

  let init = Pool {free= List.init maxId (fun x -> x + 1); used= []}

  let free (Pool pool) id =
    Pool {used= List.filter (fun x -> x = id) pool.used; free= id :: pool.free}

  let get (Pool pool) =
    match pool.free with
    | id :: ids ->
        let newPool = Pool {free= ids; used= id :: pool.used} in
        (Id id, newPool)
    | [] -> (Id 1, Pool {free= List.init maxId (fun x -> x + 2); used= [1]})

  let isFree (Pool pool) (Id id) = List.mem id pool.free
end

module PresetMap = Map.Make (Int)

module PresetMemory = struct
  type preset =
    | Preset of
        { start: float
        ; slicedur: float
        ; rate: float
        ; bufnum: int
        ; amp: float
        ; pan: float
        ; gate: int }

  type t = Presets of preset PresetMap.t

  let defaultPreset =
    Preset
      { start= 0.0
      ; slicedur= 1.0
      ; rate= 1.0
      ; bufnum= 0
      ; amp= 1.0
      ; pan= 0.0
      ; gate= 1 }

  let mk_preset key_id start slicedur rate bufnum amp pan gate =
    (key_id, Preset {start; slicedur; rate; bufnum; amp; pan; gate})

  let withPitch pitch =
    let pf = float_of_int pitch in
    defaultPreset |> fun (Preset pr) -> Preset {pr with rate= Cisp.mtor pf}

  let init =
    List.init 128 (fun x -> x)
    |> List.fold_left
         (fun map k -> PresetMap.add k (withPitch k) map)
         PresetMap.empty
    |> fun m -> Presets m

  let lookup key (Presets ps) = PresetMap.find_opt key ps
end

module PlayingNotes = Map.Make (Int)

type model =
  | PlaybackManager of
      { nodeIdManager: UniqueId.t
      ; playingNotes: UniqueId.id PlayingNotes.t
      ; presetMem: PresetMemory.t }

let playFrom addr input =
  let sendSamplerMessage addr (PresetMemory.Preset p) (UniqueId.Id nodeId) =
    Lo.send addr "/s_new"
      [ `String "simpleSampler"
      ; `Int32 nodeId
      ; `Int32 0
      ; `Int32 1
      ; `String "start"
      ; `Float p.start
      ; `String "slicedur"
      ; `Float p.slicedur
      ; `String "rate"
      ; `Float p.rate
      ; `String "bufNum"
      ; `Int32 p.bufnum
      ; `String "amp"
      ; `Float p.amp
      ; `String "pan"
      ; `Float p.pan
      ; `String "gate"
      ; `Int32 1 ]
  in
  let stopNote addr (UniqueId.Id nodeId) =
    Lo.send addr "/n_set" [`Int32 nodeId; `String "gate"; `Int32 0]
  in
  let playNote note (PlaybackManager model) =
    let presetOpt = PresetMemory.lookup note model.presetMem in
    match presetOpt with
    | None -> PlaybackManager model
    | Some preset -> (
        let idOpt = UniqueId.get model.nodeIdManager in
        match idOpt with
        | id, idState ->
            sendSamplerMessage addr preset id ;
            PlaybackManager
              { model with
                playingNotes= PlayingNotes.add note id model.playingNotes
              ; nodeIdManager= idState } )
  in
  let stopNote note (PlaybackManager model) =
    let notesMinusStopped = PlayingNotes.remove note model.playingNotes in
    let opt = PlayingNotes.find_opt note model.playingNotes in
    match opt with
    | Some id ->
        stopNote addr id ;
        PlaybackManager {model with playingNotes= notesMinusStopped}
    | None -> () ; PlaybackManager model
  in
  let update msg model =
    match msg with
    | Midi.NoteOn (_, Pitch p, _) -> playNote p model
    | Midi.NoteOff (_, Pitch p, _) -> stopNote p model
    | _ -> model
  in
  let init =
    PlaybackManager
      { playingNotes= PlayingNotes.empty
      ; nodeIdManager= UniqueId.init
      ; presetMem= PresetMemory.init }
  in
  let eval _ = () in
  input
  |> fun inp ->
  Cisp.recursive inp init update eval |> Seq.map (fun () -> Midi.SilenceEvent)

let simpleSamplerPreset =
  return PresetMemory.mk_preset
  <*> int_par "voiceNr" <*> flt_par "start" <*> flt_par "slice"
  <*> flt_par "rate" <*> int_par "bufnum" <*> flt_par "amp" <*> flt_par "pan"
  <*> int_par "int"

let midiFun addr input =
  let map = Seq.map in
  let open Midi in
  input |> playFrom addr |> Midi.serialize |> map toRaw

let () =
  let addr = Lo.Address.create "127.0.0.1" 57110 in
  let f addr =
    Midi.playMidi (midiFun addr) Process.sample_rate ;
    while true do
      Unix.sleep 60
    done
  in
  let (_ : Thread.t) = Thread.create f addr in
  let _ =
    Sys.command "jack_disconnect system_midi:capture_2 ocaml_midi:ocaml_midi_in"
  in
  let _ =
    Sys.command
      "jack_disconnect ocaml_midi:ocaml_midi_out system_midi:playback_1"
  in
  let _ =
    Sys.command "jack_connect ocaml_midi:ocaml_midi_out system_midi:playback_1"
  in
  let _ =
    Sys.command "jack_connect system_midi:capture_2 ocaml_midi:ocaml_midi_in"
  in
  while true do
    Unix.sleep 60
  done
