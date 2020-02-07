open Process

let _ =
  (* parsing config *)
  let open Yojson.Basic.Util in
  let json = Yojson.Basic.from_file Sys.argv.(1) in
  let input_file = json |> member "input" |> to_string in
  let output_file = json |> member "output" |> to_string in
  let min_freq = json |> member "min-freq" |> to_float in
  let max_freq = json |> member "max-freq" |> to_float in
  let n_bands = json |> member "n-bands" |> to_int in
  let fluct_n = json |> member "fluct-n" |> to_int in
  let fluct_min_freq = json |> member "fluct-min-freq" |> to_float in
  let fluct_max_freq = json |> member "fluct-max-freq" |> to_float in
  let fluct_n_bands = json |> member "fluct-n-bands" |> to_int in
  (* read audio file *)
  let snd_file = Sndfile.read input_file in
  let in_snd = Sndfile.toProc snd_file 0 in
  (* calculate frequencies  *)
  let freqs = geo_from_to n_bands min_freq max_freq in
  (* analyze selected band only *)
  let band_rms =
    casc2_band (List.nth freqs fluct_n)
      (List.nth freqs (fluct_n + 1))
      0.707 in_snd
    |> rms 10.
  in
  (* create fluctuation spectrum *)
  let bank =
    casc_bank 0.707 fluct_n_bands fluct_min_freq fluct_max_freq band_rms
  in
  (* run and write audio file *)
  let snd =
    Sndfile.fromProc (Sndfile.n_frames snd_file - 1) (Sndfile.sr snd_file) bank
  in
  Sndfile.write snd output_file Sndfile.WAV_16
