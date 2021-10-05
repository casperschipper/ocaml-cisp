module S = Lo.Server

type state = State of {pitch: int}

let getPitch (State {pitch}) = Midi.mkPitchClip pitch

let state = ref (State {pitch= 60})

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

let myHandler path data =
  match path with
  | "/pitch" -> (
      let data = Array.to_list data in
      let parse msg = match msg with `Int32 n -> Some n | _ -> None in
      let optP = match data with [msg] -> parse msg | _ -> None in
      match optP with Some pitch -> state := State {pitch} | None -> () )
  | _ -> ()

let receive_osc_thread () =
  let port = 7777 in
  let s = S.create port myHandler in
  while true do
    S.recv s
  done

let ofTrigger = function
  | true -> Midi.c3 |> Midi.withPitch (getPitch !state)
  | false -> Midi.SilenceEvent

let midiFun input =
  input
  |> Seq.map (fun midi -> midi |> Midi.isNoteOn |> ofTrigger)
  |> Midi.serialize |> Seq.map Midi.toRaw

let midi_thread () = Midi.playMidi midiFun Process.sample_rate

let () =
  let t1 = Thread.create receive_osc_thread () in
  let t2 = Thread.create midi_thread () in
  List.iter Thread.join [t1; t2]
