
(*
let bangMessage addr =
  Lo.send addr "/test/" [`Float 3.141592;`String "fish";`Float 2.233;`String "cat";`Float 3.33]


let testMessage addr =
  Lo.send addr "/s_new" [`String "simpleSampler"
                    ;`Int32 (-1)
                    ;`Int32 0
                    ;`Int32 1
                    ;`String "start"
                    ;`Float 0.975785
                    ;`String "slicedur"
                    ;`Float 1.07
                    ;`String "duration"
                    ;`Int32 10
                    ;`String "rate"
                    ;`Float 1.09259
                    ;`String "bufNum"
                    ;`Int32 0
                    ;`String "amp"
                    ;`Float 0.01
                    ;`String "pan"
                    ;`Float 0.0
    ]

let testMessage addr =
  Lo.send addr "/s_new" [`String "simpleSampler"
                    ;`Int32 (-1)
                    ;`Int32 0
                    ;`Int32 1
                    ;`String "start"
                    ;`Float 0.975785
                    ;`String "slicedur"
                    ;`Float 1.07
                    ;`String "duration"
                    ;`Int32 10
                    ;`String "rate"
                    ;`Float 1.09259
                    ;`String "bufNum"
                    ;`Int32 0
                    ;`String "amp"
                    ;`Float 0.01
                    ;`String "pan"
                    ;`Float 0.0
    ]
 *)
module UniqueId = struct
  type t =
    Pool of   { free : int list
              ; used : int list }

  type id =
    Id of int

  let maxId = 65536
  
  let init =
    Pool { free = List.init maxId (fun x -> x + 1)
    ; used = [] }

  let free (Pool pool) id =
    Pool
      { used = List.filter (fun x -> x = id) pool.used
      ; free = id :: pool.free }

  let get (Pool pool) =
    match pool.free with
    | id :: ids ->
       let newPool =
         Pool { free = ids
              ; used = id :: pool.used }
       in
       (Id id,newPool)
    | [] -> (Id 1,Pool
         { free = List.init maxId (fun x -> x + 2)
         ; used = [1] })
    
  let isFree (Pool pool) (Id id) =
    List.mem id pool.free  
end

module PresetMap = Map.Make(Int)

module PresetMemory = struct
  type preset =
    Preset of {start : float
               ;slicedur : float
               ;rate : float
               ;bufnum : int
               ;amp : float
               ;pan : float
               ;gate: int
              }

  type t =
    Presets of preset PresetMap.t

   let defaultPreset =
    Preset { 
     start = 0.0
    ; slicedur = 1.0
    ; rate = 1.0
    ; bufnum = 0
    ; amp = 1.0
    ; pan = 0.0
    ; gate = 1
      }

   let withPitch pitch =
     let pf = float_of_int pitch in
     defaultPreset
     |> fun (Preset pr) -> Preset { pr with rate = Cisp.mtor pf }
  
  let init =
    List.init 128 (fun x -> x)
    |> List.fold_left
         (fun map k ->
           PresetMap.add k (withPitch k) map) PresetMap.empty |> fun m -> Presets m

  let lookup key (Presets ps) =
    PresetMap.find_opt key ps

end    



module PlayingNotes = Map.Make(Int)

type model = PlaybackManager of {
    nodeIdManager : UniqueId.t
   ;playingNotes : UniqueId.id PlayingNotes.t
   ;presetMem : PresetMemory.t
    }


      
let playFrom addr input =
  let sendSamplerMessage addr (PresetMemory.Preset p) (UniqueId.Id nodeId) =
    Lo.send addr "/s_new" [`String "simpleSampler"
                    ;`Int32 nodeId
                    ;`Int32 0
                    ;`Int32 1
                    ;`String "start"
                    ;`Float p.start
                    ;`String "slicedur"
                    ;`Float p.slicedur
                    ;`String "rate"
                    ;`Float p.rate
                    ;`String "bufNum"
                    ;`Int32 p.bufnum
                    ;`String "amp"
                    ;`Float p.amp
                    ;`String "pan"
                    ;`Float p.pan
                    ;`String "gate"
                    ;`Int32 1]
  in
  let stopNote addr (UniqueId.Id nodeId) =
    Lo.send addr "/n_set" [`Int32 nodeId
                         ; `String "gate"
                         ; `Int32 0]
  in
  let playNote note (PlaybackManager model) =
    let presetOpt =
      PresetMemory.lookup note model.presetMem
    in
    match presetOpt with
    | None -> (PlaybackManager model)
    | Some preset ->
       let idOpt =
         UniqueId.get model.nodeIdManager
       in
       match idOpt with
        (id,idState) ->
          sendSamplerMessage addr preset id;
          PlaybackManager
            { model with
              playingNotes = PlayingNotes.add note id model.playingNotes
            ; nodeIdManager = idState }
  in
  let stopNote note (PlaybackManager model) =
    let notesMinusStopped = PlayingNotes.remove note model.playingNotes in
    let opt = PlayingNotes.find_opt note model.playingNotes in
    match opt with
    | Some id -> stopNote addr id; PlaybackManager { model with playingNotes = notesMinusStopped }
    | None -> () 
            ; PlaybackManager model
  in
  let update msg model =
    match msg with
    | Midi.NoteOn (_,(Pitch p),_) -> playNote p model
    | Midi.NoteOff (_,(Pitch p),_) -> stopNote p model
    | _ -> model
  in
  let init = PlaybackManager {
                 playingNotes = PlayingNotes.empty
                ;nodeIdManager = UniqueId.init
                ;presetMem = PresetMemory.init
               }
  in
  let eval _ =
    ()
  in
  input
  |> fun inp -> Cisp.recursive inp init update eval
                |> Seq.map (fun () -> Midi.SilenceEvent)


let midiFun addr input =
  let map = Seq.map in
  let open Midi in
  input
  |> (playFrom addr)
  |> Midi.serialize
  |> map toRaw
  

let () =
  (* let open Osc_unix.Udp in
  let localhost = Unix.inet_addr_of_string "127.0.0.1" in
  let port = 5665 in
  let addr = Unix.ADDR_INET (localhost, port) in
  let client = Client.create () in
   *)
   let addr = Lo.Address.create "127.0.0.1" 57110 in
    
   let f addr = 
    Midi.playMidi (midiFun addr) Process.sample_rate 
    ;while true
     do Unix.sleep 60 done
   in
  let (_:Thread.t) = Thread.create f addr in
  
  let _ = Sys.command "jack_disconnect system_midi:capture_2 ocaml_midi:ocaml_midi_in" in
  let _ = Sys.command "jack_disconnect ocaml_midi:ocaml_midi_out system_midi:playback_1" in
  let _ = Sys.command "jack_connect ocaml_midi:ocaml_midi_out system_midi:playback_1" in
  let _ = Sys.command "jack_connect system_midi:capture_2 ocaml_midi:ocaml_midi_in" in
  while true
  do
    Unix.sleep 60 
  done
 
