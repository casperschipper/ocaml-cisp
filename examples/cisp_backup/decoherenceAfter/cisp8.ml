open Cisp
(*open Seq*)
(* open Format   *)

let () =
  let testSig = (st 1.0) +.~ (osc (st  220.0)) in
  Jack.playSeqs 1 Process.sample_rate [effect masterClock (Process.inputSeq 0 +.~ testSig |> leakDC 0.5) ]
                                         

