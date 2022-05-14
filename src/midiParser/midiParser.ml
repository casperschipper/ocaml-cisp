(* 

0 7 . . . . 4 . . . 2 0 2 . 12

* notes:

things that analyse will build up state.
Some may have some shared state ?
Maybe there can be a container of analysers

let analyse =
   Analyser.build [ intervals, pitchmap ]

how to fetch the analysers results ?

what is an analyser ?

Examples of analysers:

history
min
max
average
most common
tendency
direction
pattern
division
markov model

Incomplete analysis will lead to ambiguity, which can be used generativaly.

History is probably always useful as is markov model.
Counterfactual history


*)

module History = struct
  type t =
    | PitchHistory of
        { max_size: int
        ; mem: int list
              (* todo replace with https://ocaml.janestreet.com/ocaml-core/109.55.00/tmp/core_kernel/Dequeue.html *)
        }

  (* actually max max_size *)
  let empty max_size = PitchHistory {max_size; mem= []}

  let rec take n lst =
    match (n, lst) with
    | n, x :: xs -> x :: take (n - 1) xs
    | 0, _ -> []
    | _, [] -> []

  let rec drop_last lst =
    match lst with
    | [] -> []
    | [_] -> []
    | x :: xs -> x :: drop_last xs

  let update pitch_opt (PitchHistory {max_size; mem}) =
    match pitch_opt with
    | Some pitch ->
        let m3 =
          pitch :: mem
          |> fun m2 -> if List.length m2 > max_size then drop_last m2 else m2
        in
        PitchHistory {max_size; mem= m3}
    | None -> PitchHistory {max_size; mem}

  let getAll (PitchHistory {mem; _}) = mem

  let list_unique comparator lst =
    lst |> List.sort comparator
    |> List.fold_left
         (fun acc x ->
           match acc with
           | [] -> x :: acc
           | y :: _ -> if x = y then acc else x :: acc )
         []

  let getUnique ph = getAll ph |> list_unique Int.compare

  let getInterval ph =
    let rec intervalHelper lst =
      match lst with
      | [] -> []
      | [_] -> []
      | a :: b :: xs -> (b - a) :: intervalHelper (b :: xs)
    in
    getAll ph |> intervalHelper |> list_unique Int.compare
end

module Reader = Midi.Reader

type midiMessage = Midi.midiMessage

type parserState =
  | MidiAnalyserState of {currentMidi: Midi.MidiState.t; history: History.t}

(* raw midi message seq.t -> midi state seq.t  *)

module MidiState = Midi.MidiState

(*

midi seq.t -> [ map Analyse ] -> state seq.t

state =
   playing notes
   controller values
   *)

module MidiParser = struct
  type 'a t = MidiParser of (parserState -> ('a, string) Result.t)

  let emptyState =
    MidiAnalyserState
      {currentMidi= Midi.MidiState.empty; history= History.empty 4}

  (* history keeps last n pitches that where played *)

  let run (MidiParser mpf) midi_mess_sq =
    let update midi_msg (MidiAnalyserState {currentMidi; history}) =
      let opt_pitch =
        Reader.run MidiState.getCurrentNote currentMidi
        |> Option.map (fun (_, Midi.Pitch p, _) -> p)
      in
      let newHistory = History.update opt_pitch history in
      let newMidi = MidiState.update midi_msg currentMidi in
      MidiAnalyserState {currentMidi= newMidi; history= newHistory}
    in
    Cisp.recursive midi_mess_sq emptyState update mpf

  let getUnique =
    let f (MidiAnalyserState {history; _}) =
      History.getUnique history |> Result.ok
    in
    MidiParser f

  let getIntervals =
    let f (MidiAnalyserState {history; _}) =
      history |> History.getInterval |> Result.ok
    in
    MidiParser f
  (*
   0 3 7 0 4 7 0 3

0 [3 4] 7

0 1 2 2 3

  3 4 5 1

  2 0 7 12
   
*)
end
