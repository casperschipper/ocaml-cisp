open Plugface

module Myplugin:PLUG = struct
  let pattern = Seq.repeat 30
end

let () =  
 p := Some (module Myplugin:PLUG)

