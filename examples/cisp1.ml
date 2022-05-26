open Cisp


let () = 
  let env = triangle (st 50.0) |> Seq.map (linlin (-1.0) 1.0 400.0 12000.0) in
  let oscil () = osc env in
  Jack.playSeqs 0 Process.sample_rate [effect masterClock (oscil ());oscil ();oscil ();oscil ()]
