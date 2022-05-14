open Cisp

let pGen = statyPulseGen (tline (st 10.0) (seq [0.; 10000.]))

let timed = effect masterClock pGen

let () = Jack.playSeqs 1 Process.sample_rate [timed]
