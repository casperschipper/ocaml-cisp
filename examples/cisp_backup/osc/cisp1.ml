open Cisp
open Midi

(*
control

 <pre><code>
----------begin_max5_patcher----------
593.3ocuWtsiaBCDF9Z3ovhqSW4CbHouJUUqLAqTuELQFy1TsZe2q8XftsgP
HD1bCVdvw+9yy3eSdKLHJu9jnIB8Uz2PAAuEFD.gbAB55GDUwOsuj2.CKRI9
Uc9KQa7uxHNY7gqMh5VCJGQ6e2QtY+OjpCOqE6MdIXwrmvaPzcacMo6fNzmv
nu28aTsURUov.Zw5BJK.Irx9ExvraGoUu9ghcAeOLz8XyLI4mMkxBgteB8yl
42GE90ZjTYh13aFVeiwDEXhPhcMLVpqIg8AnppKf4jLFjzygb2e2A07JgQne
Vn34kvjfGke5B3uRzzvOHNKURnaQoXDAiu3VyjaHz3LHuRnyIIOB+YimiIKf
wKTs1HJsKWjExKhXNWcvk96amDYRLj7IY.4o3attNdbjYK.YS8gCkhIqpmFE
e4LCHhN7bbTHmiB81qdWTlssJehCuCotIqTIP9JCfL4JYsQPc6i4f5EJhqjE
R6LOgcKIIA7jHfca2wxaAvz0ys8tKKA2D1tEVUl7XpJunmZb7c4o1YvrbOU1
mtmpcSUWiHzjosTmi2CFJaSY2LkjqSILWQkR0++YOv5wE+eQuotUuumftamQ
jgUTgnwHUbirV8gw39LE2fFc28VDBOGgv2oPamiPq.PYyPmzUfmjYxy8pS7L
pDxVIctFOrUPG1CJ+PmgNwqfNjYny4me7FD7iGeUna5FLHg0E7kZsq61MPWo
x2EttIRKdU1OdhODWac3LV6sVMrthNk5+PO3uDnUsxN2IKcVMAKVk81olibO
HfSb36g+ATfzf3I
-----------end_max5_patcher-----------
</code></pre>
 
*)

let map = Seq.map

let midiReader =
  let boolR = MidiState.boolFromChannelR (mkChannelClip 1) in
  let depReaderR = MidiState.getDepressedR (mkChannelClip 2) |> Reader.map (List.map (fun (Midi.Pitch p,_)->  p)) in
  Reader.map2 pair boolR depReaderR
  
let currentChord = ref [60;64;67]

type arpy =
  { ix : int
  ; value : int option }

let arpeggiator notesListSq =
  let f lst { ix; _ } =
    match List.nth_opt lst ix  with
    | Some v -> { value = Some v ; ix = ix + 1 }
    | None -> { value = List.nth_opt lst 0 ; ix = 1 }
  in
  let eval { value; _ }  =
    value
  in
  recursive1 notesListSq { ix = 0; value = None } f eval
 
let notes =
  let chords =
    ofRef currentChord
  in
  st makeNoteOfInts 
  <*> (arpeggiator chords |> map (Option.value ~default:60))
  <*> (st 60) 
  <*> (seci 0.1 |> st)
  <*> (st 0)

let ofTrigger trigger =
  let handleInput (trig,chrd) =
    currentChord := chrd
    ;trig
  in
  weavePattern (map handleInput trigger) notes (st SilenceEvent)

let printChord =
  print_int (!currentChord |> List.length);
  print_string "--chord\n";
  flush stdout
  
  

let midiFun input =
  input 
  |> MidiState.makeSeq
  |> map (Reader.run midiReader)
  |> (fun trigger -> effectSync (st printChord) (ofTrigger trigger))
  |> serialize 
  |> map toRaw 
  
let () =
  let f () =
    Midi.playMidi midiFun Process.sample_rate 
    ; while true
      do
        Unix.sleep 60
      done
  in
  let _ = Thread.create f () in
  let _ = Sys.command "jack_disconnect system_midi:capture_2 ocaml_midi:ocaml_midi_in" in
  let _ = Sys.command "jack_disconnect ocaml_midi:ocaml_midi_out system_midi:playback_1" in
  let _ = Sys.command "jack_connect ocaml_midi:ocaml_midi_out system_midi:playback_1" in
  let _ = Sys.command "jack_connect system_midi:capture_2 ocaml_midi:ocaml_midi_in" in
  while true
  do
    Unix.sleep 60
  done

   

    
    


  
  
            
 
