open Process

let _ =
  let str = kuramoto [0.01; 1.9] 0.00001 0.0005 |> evert ||> impulse in
  Jack.play 0 sample_rate str
