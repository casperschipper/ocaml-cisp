open Plugface

(* module Myplugin:PLUG = struct
     let pattern = Seq.repeat 30
   end *)

let () = 
  let _ = print_endline "version 17" in
  let pattern = 
    Cisp.listWalk [| 60; 72; 20; 55 |] (Cisp.seq [| -1; 0; 0; 0; 1 |]) ()
  in
  p := Some pattern
    