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
(** plays midi, right now just a testing function *)
