

let noisysine () =
  let open Cisp in
  let env = tline_start 0.0 (seq [ 1.0; 1.0 ;1000.0 ]) (seq [ 1.0; 0.0; 0.0 ]) in
  sawtooth
    (st 349.2 *.~ (slowNoise (st 1.0) |> Seq.map (linlin (-1.0) 1.0 0.999 1.001)))
  |> ( *.- ) 0.1 |> ( *.~ ) env

let () =
  Jack.playSeqs 0 Process.sample_rate [Cisp.effect Cisp.masterClock (noisysine ());noisysine () ]
 