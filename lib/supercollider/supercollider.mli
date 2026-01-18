(* This is a module for controlling a Supercollider synth from ocaml It uses
    actual timestamps to provide more accurate timing. At this moment, I only
    support simple synths, where you provide all the parameters at the start of
    the event, instead of updating parameters after the synth has been sent. 
    It does not manage synth id's (as we don't need to contact the synth after).
    So resourse management (nr of synths playing simultaneously is up to you).

*)

(* type of argument, string, int or float *)
type osc_arg = I of int | F of float | S of string

(* encode arguments as bytes *)
val encode_osc_string : string -> bytes

val encode_osc_int32 : int32 -> bytes

val encode_osc_float : float -> bytes

(* encode an OSC message, a message is an address and a bunch of arguments *)
val encode_osc_message : address:string -> args:osc_arg list -> bytes

(* go from unix time to NTP timestamp used by OSC *)
val ntp_time_of_unix : float -> int64

(*a bundle is a list of OSC messages with a single timestamp *)
val encode_osc_bundle : time:float -> messages:bytes list -> bytes

type sender = {sock: Unix.file_descr; addr: Unix.sockaddr}

(* initialise the OSC protocol sender *)
val init_sender : ip:string -> port:int -> sender

val close_sender : sender -> unit

val send_message : sender -> bytes -> unit

val send_bundle : sender -> time:float -> messages:bytes list -> unit

(* helper method for sending a general synth *)
val send_synth :
     float
  -> sender
  -> name:string
  -> synth_id:int
  -> add_action:int
  -> target:int
  -> params:(string * float) list
  -> unit

type newSynth =
  | NewSynth of
      { synth_name: string
      ; synth_id: int
      ; add_action: int
      ; target: int
      ; params: (string * osc_arg) list }

val synth_with_pars :
  string -> (string * osc_arg) list -> newSynth

val params_to_bytes : (string * osc_arg) list -> osc_arg list

val to_args : newSynth -> osc_arg list

(* this is an exmaple synth, probably a simple sinewave with an envelope with a freq and duration *)
val simple_tone : time:float -> freq:float -> dur:float -> pos:float -> bytes

val send_multiple_synths :
     float
  -> sender
  -> (string * int * int * int * (string * float) list) list
  -> unit

val simple_jv :
  out:int -> time:float -> dur:float -> amp:float -> offset:int -> transpose:int -> bytes
  


