(** Jack audio backend for [Process]. *)

open Seq

(* 
 * output
 * input
 * process n samples
 * number of channels (out, in)
 * samples rate callback
 *)

(* returns unit *)

val playMidi : (int * int * int -> int * int * int) -> float ref -> unit

(* Midi output based on input. You take in input and return output. Second arg can be used to receive samplerate.
 *)
