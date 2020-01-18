(** Jack audio backend for [Process]. *)

val play : int -> float ref -> float Seq.t list -> unit
(** [play n_inputs Process.sample_rate processes] creates a jack audio client
    and plays back the processes. Each process in the process list will be used to 
    generate the signal for one output channel. The number of inputs has to set and 
    the sample rate should be the global variable [sample_rate] 
    from the [Process] module. *)
