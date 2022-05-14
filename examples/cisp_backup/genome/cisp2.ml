
open Cisp

let map = Seq.map

let noiseLen = 16384

let arr = Array.init noiseLen (fun _ -> rvfi (-0.5) 0.5)

let snippetLen = 128

let len = noiseLen / snippetLen

let oneVoice () =
  let genome = rangei 0 len |> Array.of_seq in
  let phasors =
    let onePhase (start : int) = rangei start (start + snippetLen) in
    let starts = index genome (wrappedCount genome) in
    starts |> andThen onePhase
  in
  let generator = ch genome in
  let writer =
    let ri = rv (st 0) (st <| Array.length genome) in
    zip ri generator
    |> map (fun (i, v) -> mapIndex genome i (fun _ -> v))
    |> syncEffectClock (interval (rv (st 0) (st 30)))
  in
  effect writer (index arr phasors) *.~ st 40.0

let timed = effect masterClock (mkLots 40 oneVoice)

let () = Jack.playSeqs 1 Process.sample_rate [timed; mkLots 40 oneVoice]
