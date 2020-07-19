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

val playMidi :
  (int * int * int) Seq.t -> (int * int * int) Seq.t ref -> float ref -> unit

(* Midi output based on input. You provide a seq of functions to generate output based * on input. Second arg can be used to receive samplerate.
 *)
