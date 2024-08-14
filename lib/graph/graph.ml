(* the model is an array. Each slot in the array has an index generator and a segment generator *)

open Infseq

type model =
  | Model of { nodes : (int Infseq.t * float Seq.t Infseq.t) Array.t; current : int }

let update () (Model { nodes; current }) =
  let indexer, segments = nodes.(current) in
  let new_index, index_tail =
    match indexer () with
    | Infseq.inf_node 
  in
  ; nodes.(current) <- (
  (0.0, { nodes = nodes; current = new_index })
