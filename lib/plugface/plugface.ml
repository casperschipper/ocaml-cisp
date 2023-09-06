module type PLUG =
  sig
    val pattern : int Seq.t
  end
  
let p = ref None
let get_plugin () : (module  PLUG)  =
  match !p with 
  | Some s -> s
  | None -> failwith "No plugin loaded"