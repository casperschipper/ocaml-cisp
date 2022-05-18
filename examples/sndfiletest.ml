open Process

(* reading the first channel of an audio file *)
let _ =
  (* replace with another  audio file *)
  let snd = Sndfile.read "/Users/casperschipper/Downloads/tmp/applaud.wav" in
  let sndproc = Sndfile.toProc snd 0 in
  let filtered = bhpf_static 1350. 1. sndproc *~ ~.0.3 in
  Jack.play 0 Process.sample_rate [filtered]
