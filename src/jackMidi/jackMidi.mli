(** Jack midi backend *)

val playMidi : (int * int * int -> int * int * int) -> float ref -> unit

(* playmidi f sample_rate *)

(* Midi output based on input. You take in input and return output. Second arg can be used to receive samplerate.
 *)
