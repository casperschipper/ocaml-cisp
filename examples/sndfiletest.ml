open Process

let _ =
  (* replace with another 1-channel audio file *)
  let snd = sndfile "/home/luc/Work/sounds/aurora.wav" in
  let filtered = bhpf_static 1350. 1. snd *~ ~.0.3 in
  Jack.play 0 Process.sample_rate [filtered]
