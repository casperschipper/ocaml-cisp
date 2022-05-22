open Cisp

let () = 
  let env = tline (st 0.0001) (seq [0.0;1.0;1.0;0.0]) in
  let _ = effect masterClock (env) |> take 50 |> Seq.iter (fun x -> print_float x; print_newline (); flush stdout)  in ()
