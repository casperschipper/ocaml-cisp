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
(*
Would be very cool:

if input mixes dry sounds with echo, use more speakers.


*) 
let mkBoermanFading3 () =
  let open Cisp in
  let memsize = 10.0 in
  let tabIndex = seq [0.0;sec memsize]  in
  let dura = ch [|0.5;1.0;0.666667;1.5;2.0|] |> Seq.map (fun x -> x *. memsize)  in
  let readpos = tline dura tabIndex |> Seq.map (clip 0.0 (sec memsize)) in
  let buffer = Array.make (seci memsize) 0.0 in
  let input = (Process.inputSeq 0 +.~ Process.inputSeq 1) *.~ (st 0.5) |> bhpf_static 50.0 0.9  in
  let writerIdx = countTill <| cap buffer in
  let writer = write buffer writerIdx input in
  let myReader =
    indexCub buffer readpos 
  in
  let joined = effect writer myReader in
  2.0 *.- joined |> Seq.map tanh 
 
let all_channels =
  Cisp.rangei 0 15 |> List.of_seq |> List.map (fun _ -> mkBoermanFading3 ())

let () =
  let open Cisp in
  let f () =
    let () =
      Jack.playSeqs 2 Process.sample_rate
        (effect masterClock (mkBoermanFading3 ()) :: all_channels)
    in
    while true do
      Unix.sleep 60
    done
  in
  let _ = Thread.create f () in
  let _ =
    List.map
      (fun str -> Sys.command str |> ignore)
      [ "jack_connect system:capture_5 ocaml:input_0"; "jack_lsp -c"; "jack_connect system:capture_1 ocaml:input_1" ]
  in
  while true do
    Unix.sleep 60
  done
