
let mkBoermanFading2 =
  let open Cisp in
  let map = Seq.map in
  let tabIndex =
    (ch [|0.0;1.0;2.0;0.5;1.99|]) 
  in
  let dura =
    (ch [|0.1;0.001|])
  in       
  let myLineTest =
    tline dura tabIndex
  in
  let bufIndex = myLineTest |> map (( *. ) <| sec 4.0) in
  let taper fadeTime x =
    match x with
    1.0 -> 1.0
    | att when x < fadeTime -> att /. 0.05
    | rel when x > (1.0 -. fadeTime) -> 1.0 -. ((rel -. (1.0 -. fadeTime)) /. fadeTime)
    | _ -> 1.0
  in
  let buffer = Array.make (seci 5.0) 0.0 in 
  let input = Process.inputSeq 0 |> bhpf_static 10.0 0.9 in
  let writerIdx = (countTill <| cap buffer) in
  let env = writerIdx |> map Float.of_int |> map (fun x -> x /. (cap buffer |> Float.of_int) |> taper 0.05) in
  let mupInput = input *.~ env in
  let writer =  write buffer writerIdx mupInput in
  let myReader = (indexCub buffer bufIndex) *.~ (myLineTest |> map (taper 0.05 )) in
  let joined = effect writer myReader in
  joined 

let () = 
  let open Cisp in
  let f () =
    let () = Jack.playSeqs 1 Process.sample_rate [ effect masterClock (mkBoermanFading2) ; mkBoermanFading2 ] in
    while true do
      Unix.sleep 60
    done
  in
  let _ = Thread.create f () in
  let _ = List.map (fun str -> Sys.command str |> ignore)
    [
    "jack_connect system:capture_1 ocaml:input_0";
    "jack_lsp -c"]
  in 
  while true do
    Unix.sleep 60
  done