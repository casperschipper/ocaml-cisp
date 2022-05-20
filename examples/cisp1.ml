open Cisp

let () = 
  let noise = rvf (st (-1.0)) (st 1.0) in
  let filtered = bbpf_static 100.0 0.9 noise in 
  Jack.playSeqs 2 Process.sample_rate [noise;filtered]
