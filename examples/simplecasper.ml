open Process

let _ =
  let stream = sinosc ~.330. in
  Jack.play 0 Process.sample_rate [stream]
