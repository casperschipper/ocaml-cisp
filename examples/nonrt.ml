open Process

(* writing an audio file in nonrt *)
let _ =
  let bank = fbank_subtract blpf_static 0.707 20 30. 20000. (rnd *~ ~.0.1) in
  let snd = Sndfile.fromProc (44100 * 20) 44100 bank in
  (* adjust path *)
  Sndfile.write snd "/home/luc/Work/sounds/filtered1.wav" Sndfile.WAV_16
