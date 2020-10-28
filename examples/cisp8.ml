open Cisp
(*open Seq*)
(* open Format   *)

let () =
  Jack.playSeqs 1 Process.sample_rate [effect masterClock (Process.inputSeq 0 +.~ (st 1.0) |> leakDC 0.995) ]
                                         

