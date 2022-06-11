

let () =
  let open Cisp in
  let arr = lift rvf 1.0 1000.0 |> take 19 in
  let signal freq = 
    let amp = mupWalk 1.0 (st 1.0) in 
    pulsegen (st freq) *.~ amp in
  let channels = arr |> fmap signal |> List.of_seq |> sumlist |> fun x -> [x;x] in
  let with_effect = match channels with
    |  first ::rest -> effect masterClock first :: rest
    | [] -> []
  in
  if false then
    let size = !Process.sample_rate *. 90.0 |> int_of_float in
    let t = Sndfile.from_seq size (int_of_float !Process.sample_rate) with_effect in
    Sndfile.write t "/Users/casperschipper/Music/Null/pulse_split_recycle_range.wav" Sndfile.WAV_32
  else 
    Jack.playSeqs 0 Process.sample_rate with_effect
