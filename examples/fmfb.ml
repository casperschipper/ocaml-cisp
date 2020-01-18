open Process

let _ =
  let stream = fm_feedback (~.139.5, ~.12.2) (~.22.10, ~.11.2) |> split in
  Jack.play 0 Process.sample_rate stream
