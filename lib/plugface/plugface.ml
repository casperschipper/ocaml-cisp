(* module type PLUG =
  sig
    val pattern : int Seq.t
  end *)
  
(* some comment *)

let p = ref None
let get_plugin () : int Seq.t  =
  match !p with 
  | Some s -> s
  | None -> failwith "No plugin loaded"