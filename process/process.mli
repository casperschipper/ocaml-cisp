(** An audio synthesis library based on OCaml's [Seq] type. *)

(** {2 Globals} *)

val sample_rate : float ref
(** All processes that need to take the current sample rate into
   account need to refer to this variable.*)

val input_array : float array
(** The array of input values. This should accessed using the [input] function. *)

val input_channels : int ref
(** The number of input channels*)

val pi : float

val two_pi : float

(** {2 Creating and Accessing Processes} *)

val fromLst : 'a list -> 'a Seq.t
(** Constructs a finite process from a list of values. *)

val toLst : int -> 'a Seq.t -> 'a list
(** [toLst n process] constructs a list with the length [n] from the [process]. *)

val this : (unit -> 'a Seq.node) -> 'a option

val next : (unit -> 'a Seq.node) -> 'a Seq.t option

val const : 'a -> 'a Seq.t

val ( ~. ) : 'a -> 'a Seq.t

val id : float Seq.t -> float Seq.t

val from_ref : 'a ref -> 'a Seq.t

val print_float_stream : string -> float Seq.t -> float Seq.t

(** {2 Applying Functions on Processes} *)

val zip : ('a -> 'b -> 'c) -> 'a Seq.t -> 'b Seq.t -> 'c Seq.t

val map : ('a -> 'b) -> 'a Seq.t -> 'b Seq.t

(** {2 Arithmetic Operations} *)

val ( +~ ) : float Seq.t -> float Seq.t -> float Seq.t

val ( *~ ) : float Seq.t -> float Seq.t -> float Seq.t

val ( -~ ) : float Seq.t -> float Seq.t -> float Seq.t

val ( /~ ) : float Seq.t -> float Seq.t -> float Seq.t

val mul : float -> float Seq.t -> float Seq.t

val sum : float Seq.t list -> float Seq.t

(** {2 Audio Input} *)

val input : int -> float Seq.t

(** {2 Multichannel Operations} *)

val evert : 'a list Seq.t -> 'a Seq.t list

val split : ('a * 'a) Seq.t -> 'a Seq.t list

val ( |>> ) : 'a list -> ('a -> 'b) list -> 'b list

val ( ||> ) : 'a list -> ('a -> 'b) -> 'b list

val pan2 : float Seq.t -> float Seq.t -> float Seq.t list

val pan2_const : float Seq.t -> float -> float Seq.t list

val splay : float Seq.t list -> float Seq.t list

(** {2 Recursive Connections} *)

val recursive_connection :
  'a -> ('b -> 'a Seq.t) -> ('a Seq.t -> 'b) -> unit -> 'a Seq.node

val recursive : 'a -> ('a Seq.t -> 'a Seq.t) -> unit -> 'a Seq.node

val recursion_map : 'a -> ('b -> 'a) -> ('a -> 'b) -> 'a Seq.t

(** {2 Integration} *)

val integrate : float Seq.t -> unit -> float Seq.node

val inc : float -> float -> float Seq.t

(** {2 Delays} *)

val del1 : 'a -> 'a Seq.t -> 'a Seq.t

(** {2 Noise} *)

val rnd : float Seq.t

(** {2 Filters} *)

val blpf : float Seq.t -> float Seq.t -> float Seq.t -> float Seq.t

val blpf_static : float -> float -> float Seq.t -> float Seq.t

val bhpf_static : float -> float -> float Seq.t -> float Seq.t

val bbpf_static : float -> float -> float Seq.t -> float Seq.t

val lpf1 : float Seq.t -> float Seq.t -> float Seq.t

(** {2 Analysis} *)

val rms : float Seq.t -> float Seq.t -> float Seq.t

(** {2 Oscillators} *)

val sinosc : float Seq.t -> float Seq.t

val impulse : float Seq.t -> float Seq.t

val fm_feedback :
     float Seq.t * float Seq.t
  -> float Seq.t * float Seq.t
  -> unit
  -> (float * float) Seq.node

val kuramoto : float list -> float -> float -> unit -> float list Seq.node
