module S = Lo.Server
module R = Result

type preset =
  | Preset of
      { start: float
      ; slicedur: float
      ; rate: float
      ; bufnum: int
      ; amp: float
      ; pan: float }

type data = Lo.Message.data

let of_data = function
  | `Int32 n -> string_of_int n
  | `Float f
   |`Double f ->
      Printf.sprintf "%f" f
  | `String s -> s
  | `True -> "True"
  | `False -> "False"
  | _ -> "???"

let print_preset (Preset p) =
  print_string "start t:" ;
  print_float p.start ;
  print_string "end t:" ;
  print_float p.slicedur ;
  print_newline () ;
  flush stdout

let debug_handler path data =
  let data = Array.to_list data in
  let s = List.map of_data data in
  let s = String.concat ", " s in
  Printf.printf "Message on %s: %s\n%!" path s

let bang = ref false

let midiLoop input =
  input
  |> Seq.map (fun i ->
         i |> Midi.isNoteOn |> ( := ) bang ;
         Midi.MidiSilence |> Midi.toRaw )

let midi_in () = Midi.playMidi midiLoop Process.sample_rate

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
  type t = Presets of preset PresetMap.t

  type msg = Add of int * preset | Remove of int

  let defaultPreset =
    Preset {start= 0.0; slicedur= 1.0; rate= 1.0; bufnum= 0; amp= 1.0; pan= 0.0}

  let mk_preset key_id start slicedur rate bufnum amp pan =
    Add (key_id, Preset {start; slicedur; rate; bufnum; amp; pan})

  let withPitch pitch preset =
    let pf = float_of_int pitch in
    preset |> fun (Preset pr) -> Preset {pr with rate= Cisp.mtor pf}

  (*let init =
    List.init 128 (fun x -> x)
    |> List.fold_left
         (fun map k -> PresetMap.add k (defaultPreset |> withPitch k) map)
         PresetMap.empty
    |> fun m -> Presets m
  *)

  let init = Presets PresetMap.empty

  let lookup key (Presets ps) = PresetMap.find_opt key ps

  let update msg (Presets ps) =
    match msg with
    | Add (k, preset) -> Presets (PresetMap.add k preset ps)
    | Remove k -> Presets (PresetMap.remove k ps)

  let print_state (Presets ps) =
    print_endline "ok" ;
    PresetMap.iter
      (fun idx p ->
        print_string ("preset:\n" ^ string_of_int idx) ;
        print_preset p ;
        flush stdout )
      ps
end

let presetMemory = ref PresetMemory.init

let update_memory msg = presetMemory := PresetMemory.update msg !presetMemory

let simpleSamplerPreset =
  let open OscParser in
  return PresetMemory.mk_preset
  <*> int_par "voiceNr" <*> flt_par "start" <*> flt_par "slicedur"
  <*> flt_par "rate" <*> int_par "bufnum" <*> flt_par "amp" <*> flt_par "pan"

let myHandler path d =
  debug_handler path d ;
  match path with
  | "/set" -> (
      let data = Array.to_seq d in
      let parsed = OscParser.run simpleSamplerPreset data in
      match parsed with
      | R.Error error ->
          print_string (error ^ "\n") ;
          flush stdout
      | R.Ok (m, _) ->
          print_string "received message" ;
          flush stdout ;
          update_memory m )
  | "/print" ->
      PresetMemory.print_state !presetMemory ;
      flush stdout
  | _ ->
      print_string ("unknown path: " ^ path ^ "\n") ;
      flush stdout

let osc_receive () =
  let port = 7777 in
  let s = S.create port myHandler in
  while true do
    S.recv s
  done

module PlayingNotes = Map.Make (Int)

type model =
  | PlaybackManager of
      { nodeIdManager: UniqueId.t
      ; playingNotes: UniqueId.id PlayingNotes.t
      ; presetMem: PresetMemory.t ref }

let playFrom addr presetRef input =
  let sendSamplerMessage addr (Preset p) (UniqueId.Id nodeId) =
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
    let presetOpt = PresetMemory.lookup note !(model.presetMem) in
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
  let init (presetRef : PresetMemory.t ref) =
    PlaybackManager
      { playingNotes= PlayingNotes.empty
      ; nodeIdManager= UniqueId.init
      ; presetMem= presetRef }
  in
  let eval _ = () in
  input
  |> fun inp ->
  Cisp.recursive inp (init presetRef) update eval
  |> Seq.map (fun () -> Midi.SilenceEvent)

let midiFun addr input =
  let map = Seq.map in
  let open Midi in
  input |> playFrom addr presetMemory |> Midi.serialize |> map toRaw

let () =
  let addr = Lo.Address.create "127.0.0.1" 57110 in
  let f addr =
    Midi.playMidi (midiFun addr) Process.sample_rate ;
    while true do
      Unix.sleep 60
    done
  in
  let t1 = Thread.create f addr in
  let t2 = Thread.create osc_receive () in
  let _ = List.iter Thread.join [t1; t2] in
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
