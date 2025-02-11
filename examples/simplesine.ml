open Process

let _ =
  let open Cisp in
  let stream = sinosc (st 500.)in
  Jack.playSeqs 0 Process.sample_rate [ stream ]
