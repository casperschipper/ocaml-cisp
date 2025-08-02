(* osc.mli *)

(** Type representing OSC argument values *)
type osc_arg = I of int | F of float | S of string

(** Type representing a reusable UDP sender *)
type sender

(** [init_sender ~ip ~port] creates a reusable UDP socket for sending OSC messages. *)
val init_sender : ip:string -> port:int -> sender

(** [close_sender sender] closes the UDP socket. *)
val close_sender : sender -> unit

(** [send_message sender msg] sends a raw OSC message over UDP. *)
val send_message : sender -> Bytes.t -> unit

(** [send_bundle sender ~time ~messages] sends an OSC bundle scheduled at [time]. *)
val send_bundle : sender -> time:float -> messages:Bytes.t list -> unit

(** [send_synth sender ~name ~synth_id ~add_action ~target ~params]
    sends a SuperCollider /s_new message for spawning a synth.
    - [name]: SynthDef name
    - [synth_id]: Synth ID (can be -1 for auto-generated)
    - [add_action]: Add action code (e.g., 1 for add to head)
    - [target]: Target group/node ID
    - [params]: List of (name, value) parameter pairs (e.g., [("freq", 440.0)]) *)
val send_synth :
  float ->
  sender ->
  name:string ->
  synth_id:int ->
  add_action:int ->
  target:int ->
  params:(string * float) list ->
  unit
  
val send_multiple_synths :
  float ->
  sender ->
  (string * int * int * int * (string * float) list) list ->
  unit

val simple_tone :
  time:float -> freq:float -> dur:float -> bytes
  



