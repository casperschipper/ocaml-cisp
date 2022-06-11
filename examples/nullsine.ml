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
(* let noisysine =
  let open Cisp in
  let env = tline_start 0.0 (seq [ 1.0; 15.0 ; 1.0 ;1000.0 ]) (seq [ 1.0; 1.0; 0.0; 0.0 ]) in
  sawtooth
    (st 349.2 *.~ (slowNoise (st 1.0) |> Seq.map (linlin (-1.0) 1.0 0.999 1.001)))
  |> ( *.- ) 0.1 |> ( *.~ ) env *)

let readFile () = 
    let snd = Sndfile.read "/Users/casperschipper/Music/Null/tablepath2nrt.wav" in
    (* let snd_n_channels = Sndfile.n_channels snd in *)
    Sndfile.to_seq snd 1 |> Cisp.take (48000 * 90) |> List.of_seq |> Cisp.seq 

let mkBoermanFading3 () =
  let open Cisp in
  let memsize = 10.0 in
  let tabIndex = seq [ 0.0; sec memsize ] in
  let dura =
    ch [| 1.0;2.0;3.0;4.0;8.0 |]
    |> Seq.map (fun x -> x *. memsize)
  in
  let readpos = tline dura tabIndex |> Seq.map (clip 0.0 (sec memsize)) in
  let buffer = Array.make (seci memsize) 0.0 in
  let input = Process.inputSeq 0 +.~ (readFile ()) in
  let writerIdx = countTill <| cap buffer in
  let writer = write buffer writerIdx input in
  let myReader = indexCub buffer readpos in
  let joined = effect writer myReader in
  0.75 *.- joined

let all_channels =
  Cisp.rangei 0 6 |> List.of_seq |> List.map (fun _ -> mkBoermanFading3 ())

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
      [ "jack_connect system:capture_0 ocaml:input_0"; "jack_lsp -c" ]
  in
  while true do
    Unix.sleep 60
  done
