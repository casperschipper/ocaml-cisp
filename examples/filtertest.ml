open Process

let _ =
  let noise = rnd in
  (* let filtered = blpf_static 350. 10. noise in *)
  let filtered = bhpf_static 8350. 0.75 noise *~ ~.0.03 in
  (* let filtered = bbpf_static 450. 10. noise *~ ~.0.2 in *)
  Jack.play 0 Process.sample_rate [filtered]
