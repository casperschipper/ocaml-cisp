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
  let dura = ch [| 0.01; 0.125; 0.25; 0.5; 2.0/.3.0; 3.0/.2.0; 2.0;4.0 |] |> Seq.map (fun x -> x *. memsize)  in
  let readpos = tline dura tabIndex |> Seq.map (clip 0.0 (sec memsize)) in
  let buffer = Array.make (seci memsize) 0.0 in
  let input = Process.inputSeq 0 |> bhpf_static 50.0 0.9  in
  let writerIdx = countTill <| cap buffer in
  let writer = write buffer writerIdx input in
  let myReader =
    indexCub buffer readpos 
  in
  let joined = effect writer myReader in
  3.0 *.- joined 
 
let all_channels =
  Cisp.rangei 0 15 |> List.of_seq |> List.map (fun _ -> mkBoermanFading3 ())

let () =
  let open Cisp in
  let f () =
    let () =
      Jack.playSeqs 1 Process.sample_rate
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
      [ "jack_connect system:capture_1 ocaml:input_0"; "jack_lsp -c" ]
  in
  while true do
    Unix.sleep 60
  done
