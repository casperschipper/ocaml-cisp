open Process

(* writing an audio file in nonrt *)
let _ =
  let in_snd = rnd *~ ~.0.1 in
  (* let in_snd = Sndfile.toProc "/home/luc/Work/sounds/aurora.wav" 0 in *)
  (* let bank = fbank_subtract blpf_static 0.707 80 20. 20000. in_snd in *)
  (* let snd = Sndfile.fromProc (44100 * 20) 44100 [sum bank -~ noise] in *)
  (* let rms_lst = bank ||> rms 10. in
   * let snd = Sndfile.fromProc (44100 * 20) 44100 rms_lst in *)
  (* let snd = Sndfile.fromProc (44100 * 20) 44100 [bank |> sum] in *)
  (* let snd = Sndfile.fromProc (44100 * 20) 44100 [rms 10. in_snd] in *)
  let bank = casc_bank 0.707 20 20. 20000. in_snd in
  let snd = Sndfile.fromProc (44100 * 20) 44100 bank in
  (* adjust path *)
  Sndfile.write snd "/home/luc/Work/sounds/filtered1.wav" Sndfile.WAV_16
