type param = IntParam of int | FloatParam of float | StringParam of string

let intPar i = IntParam i

let floatPar f = FloatParam f

let stringPar s = StringParam s

type csoundevent =
  | CSEvent of {instr: int; start: float; dur: float; params: param list}

type cscore = Cscore of csoundevent list

let mkScore evts = Cscore evts

let fromStreams instr starts durs paramLst =
  (* TODO: implement the function logic *)
  let rec aux s_starts s_durs param_seqs acc =
    match (Seq.uncons s_starts, Seq.uncons s_durs) with
    | Some (start, s_starts'), Some (dur, s_durs') ->
        (* For each param_seq in param_seqs, get the head and the tail *)
        let heads, tails, all_some =
          List.fold_right
            (fun seq (heads, tails, all_some) ->
              match Seq.uncons seq with
              | Some (p, seq') -> (p :: heads, seq' :: tails, all_some)
              | None -> (heads, tails, false) )
            param_seqs ([], [], true)
        in
        if all_some then
          let event = CSEvent {instr; start; dur; params= heads} in
          aux s_starts' s_durs' tails (event :: acc)
        else List.rev acc
    | _ -> List.rev acc
  in
  Cscore (aux starts durs paramLst [])

  (** [fromArgs instr start dur offset transpose channel envDur attack decay sustain release]
    Constructs a [CSEvent] record with the given parameters.

    @param instr The instrument number or identifier.
    @param start The start time of the event.
    @param dur The duration of the event.
    @param offset The offset parameter for the event.
    @param transpose The transpose value for pitch adjustment.
    @param channel The channel number or identifier.
    @param envDur The envelope duration.
    @param attack The attack time of the envelope.
    @param decay The decay time of the envelope.
    @param sustain The sustain level of the envelope.
    @param release The release time of the envelope.
    @return A [CSEvent] record populated with the provided parameters.
  *)

let fromArgs instr start dur offset transpose channel envDur attack decay
    sustain release =
  CSEvent
    { instr
    ; start
    ; dur
    ; params=
        [offset; transpose; channel; envDur; attack; decay; sustain; release] }

let render_cscore_to_file  filename (Cscore events) =
  let param_to_string = function
    | IntParam i -> string_of_int i
    | FloatParam f -> string_of_float f
    | StringParam s -> Printf.sprintf "\"%s\"" s
  in
  let event_to_string = function
    | CSEvent {instr; start; dur; params} ->
        let params_str = String.concat " " (List.map param_to_string params) in
        Printf.sprintf "i%d %g %g %s" instr start dur params_str
  in
  let lines = List.map event_to_string events in
  let oc = open_out filename in
  List.iter (fun line -> output_string oc (line ^ "\n")) lines ;
  close_out oc
