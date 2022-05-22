open Cisp


let () = 
  let _ = Process.sample_rate := 10.0 in
  let env = tline_start 0.0 (st 1.0) (seq [1.0;0.0]) in
  let _ = effect masterClock (env) |> take 60  |> Seq.iter (fun x -> print_string" x: "; print_float x; print_newline (); flush stdout)  in ()
