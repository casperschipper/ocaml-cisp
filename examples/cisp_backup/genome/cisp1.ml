open Cisp

let map = Seq.map

let noiseLen = 16384

let arr = Array.init noiseLen (fun _ -> rvfi (-1.0) 1.0)

let snippetLen = 256

let len = noiseLen / snippetLen

let genome = rangei 0 len |> Array.of_seq

let phasors =
  let onePhase (start : int) = rangei start (start + snippetLen) in
  let starts = index genome (wrappedCount genome) in
  starts |> andThen onePhase

let idx = wrappedCount arr

let generator = ch genome

let writer =
  let ri = rv (st 0) (st <| Array.length genome) in
  zip ri generator
  |> map (fun (i, v) -> mapIndex genome i (fun _ -> v))
  |> syncEffectClock (interval (rv (st 0) (st 3000)))

let out = effect writer (index arr phasors)

let timed = effect masterClock out

let () = Jack.playSeqs 1 Process.sample_rate [timed; out]
