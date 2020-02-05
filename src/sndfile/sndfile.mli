(** Audio Utilities for OCaml *)

(** A writing and a reading functions for several audio file formats using libsndfile *)

type t

(** The output formats. *)
type format =
  | WAV_8
  | WAV_16
  | WAV_24
  | WAV_32
  | WAV_FLOAT
  | AIFF_8
  | AIFF_16
  | AIFF_24
  | AIFF_32
  | AIFF_FLOAT
  | RAW_8
  | RAW_16
  | RAW_24
  | RAW_32
  | RAW_FLOAT

val write : t -> string -> format -> unit
(** [write snd filename format] *)

val read : string -> t
(** [read filename]. This function can read files of more formats then the above. 
    (see libsndfile api for list of formats) *)

val channels : t -> int
(** [channels snd]. Returns the number of channels in the audio file. *)

val sr : t -> int
(** [sr snd]. Returns the samplerate of the audio file. *)

val idx : t -> int -> float
(** [idx snd idx] *)

val idx_channel : t -> int -> int -> float
(** [idx_channel snd channel idx] *)

val fromSeq : int -> int -> float Seq.t list -> t
(** [fromSeq length sample_rate streams ] *)

val fromProc : int -> int -> ('a, float) Process.t list -> t
(** [fromProc length sample_rate processes ] *)

val toProc : string -> int -> (unit, float) Process.t
