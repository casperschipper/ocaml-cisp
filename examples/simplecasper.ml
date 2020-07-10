open Process

let _ =
  let stream = sinosc ~.500. in
  Jack.play 0 Process.sample_rate [stream]
