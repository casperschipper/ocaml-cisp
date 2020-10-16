(** Jack audio backend for [Process]. *)

val open_stream :
     (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t
  -> (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t
  -> (int -> unit)
  -> int * int
  -> (int -> unit)
  -> unit

(* 
 * output
 * input
 * process n samples
 * number of channels (out, in)
 * samples rate callback
 *)

(* returns unit *)

val play : int -> float ref -> float Process.t list -> unit
(** [play n_inputs Process.sample_rate processes] creates a jack audio client
    and plays back the processes. Each process in the process list will be used to 
    generate the signal for one output channel. The number of inputs has to set and 
    the sample rate should be the global variable [sample_rate] 
    from the [Process] module. *)

(* this is for float Seq.t *)
val playSeqs : int -> float ref -> float Seq.t list -> unit
                                                        
