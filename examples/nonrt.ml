open Process

(* writing an audio file in nonrt *)
let _ =
  let bank = fbank bbpf_static 2. 10 20. 8000. (rnd *~ ~.0.05) in
  let snd = Sndfile.fromSeq (44100 * 10) 44100 bank in
  (* adjust path *)
  Sndfile.write snd "/home/luc/Work/sounds/filtered.wav" Sndfile.WAV_16
