let low = ref 0.0

let mini_peak p width alternative input =
  let f x alt = 
    if x > p && x < (p +. width) then 
      alt 
  else
    x
  in
  Cisp.map2 f input alternative

let mini_peak_dyn psq width alternative input =
    let f p x alt = 
      if x > p && x < (p +. width) then 
        alt 
    else
      x
    in
    Cisp.map3 f psq input alternative



let mkLookup () =
  let open Cisp in
  let freq = ((Random.int 8) + 1) * 25 |> float_of_int |> fun x -> x *. (pickOne [|1.0;1.5;2.0/.3.0|]) |> st |> fun x -> x *.~ (slowNoise (st 0.2) *.~ (st 0.03) +.~ (st 1.0)) in
  let input = Process.inputSeq 0 +.~ osc  freq in
  (*let arr = sineseg 16384 |> Array.of_seq in
    let distort = lookup arr input in*)
  let dura = ch [| 3.0; 5.0; 7.0 |] in
  let amps = cycle (seq [ 0.0; 0.0; 0.01; 0.0; 0.1; 0.0; 1.0 ]) in
  let envelope = tline_start 0.0 dura amps in
  (* let env2 = tline (lift rvf 4.0 33.0) (seq [0.001;0.1; 3.0; 1.0 ]) in *)
  let distort inp = Seq.map sin inp in
  input *.~ envelope |> distort |> mini_peak_dyn (0.1 *.- slowNoise (st 0.01)) 0.01 (input) |> mup (st 0.05) 
 

let all_channels =
  Cisp.rangei 0 14 |> List.of_seq |> List.map (fun _ -> mkLookup ())

let () =
  let open Cisp in
  let f () =
    let () =
      Jack.playSeqs 1 Process.sample_rate
        (effect masterClock (st (0.0)) :: (st 0.0) :: all_channels)
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
