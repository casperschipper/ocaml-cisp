(** Audio Utilities for OCaml *)
(** A writing and a reading functions for several audio file formats using libsndfile *)


(** The output formats. *)
type snd_format =
    WAV_8
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
      
(** [snd_write array filename sample_rate channels format] *)
val snd_write :
  (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t ->
  string -> int -> int -> snd_format -> unit
  
(** [snd_read filename]. This function can read files of more formats then the above. 
    (see libsndfile api for list of formats) *)
val snd_read :
  string ->
  (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t

(** [snd_channels filename]. Returns the number of channels in the audio file. *)
val snd_channels :
  string -> int

(** [snd_sr filename]. Returns the samplerate of the audio file. *)
val snd_sr :
  string -> int

    
