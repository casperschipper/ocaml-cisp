(*
let mkBoermanFading2 () =
  let open Cisp in
  let memsize = 10.0 in
  let tabIndex = seq [0.0;sec memsize]  in
  let dura = ch [| 0.5; 2.0/.3.0; 3.0/.2.0; 2.0 |] |> Seq.map (fun x -> x *. memsize)  in
  let readpos = tline dura tabIndex in
  let buffer = Array.make (seci memsize) 0.0 in
  let input = Process.inputSeq 0 |> bhpf_static 100.0 0.9 in
  let writerIdx = countTill <| cap buffer in
  let writer = write buffer writerIdx input in
  let myReader =
    indexCub buffer readpos 
  in
  let joined = effect writer myReader in
  joined 
*)

let mkLookup () =
  let open Cisp in
  let freq = ((Random.int 10) + 1) * 100 |> float_of_int |> st |> fun x -> x *.~ (slowNoise (st 0.01) *.~ (st 0.03) +.~ (st 1.0)) in
  let input = Process.inputSeq 0 +.~ osc  freq in
  (*let arr = sineseg 16384 |> Array.of_seq in
    let distort = lookup arr input in*)
  let dura = ch [| 3.0; 5.0; 7.0 |] in
  let amps = seq [ 0.0; 0.01; 0.0; 0.1; 0.0; 1.0 ] in
  let envelope = tline dura amps in
  let env2 = tline (seq [ 1.0; 11.0; 200.0 ]) (lift rvf 1.0 8.0) in
  let distort inp = Seq.map sin inp in
  input *.~ envelope *.~ env2 |> distort |> Seq.map tanh

let all_channels =
  Cisp.rangei 0 15 |> List.of_seq |> List.map (fun _ -> mkLookup ())

let () =
  let open Cisp in
  let f () =
    let () =
      Jack.playSeqs 1 Process.sample_rate
        (effect masterClock (mkLookup ()) :: all_channels)
    in
    while true do
      Unix.sleep 60
    done
  in
  let _ = Thread.create f () in
  let _ =
    List.map
      (fun str -> Sys.command str |> ignore)
      [ "jack_connect system:capture_1 ocaml:input_0"; "jack_lsp -c" ]
  in
  while true do
    Unix.sleep 60
  done
