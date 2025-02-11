let rec clone sq () =
  match sq () with
  | Seq.Nil -> Seq.Nil
  | Seq.Cons (x, xs) -> Seq.Cons ((x, x), clone xs)

let pulses d () = 
  (* very generic pulses generator *)
  let open Cisp in
  let others = funk (fun () -> Toolkit.rvfi (-1.0) 1.0) |> Seq.take 8 |> List.of_seq in
  let vs = (0.0,d)::(others |> List.map (fun v -> (v, Toolkit.rvi 1 8))) in
  weights vs

let snips () = 
  let open Cisp in 
  let d () = Toolkit.choice 10 [1;2;3;4;5;10;128;10000;200000] in
  uzi 256 (fun _ -> pulses (d ()) () |> Seq.take (Toolkit.choice 10 [10;50;60;100;20;30]) |> List.of_seq) |> Array.of_seq

let snippers () = 
  let open Cisp in
  let arr = snips () in
  let max = Array.length arr in
  let idx = rv (st 0) (st (max - 1)) |> hold (ch [|2;3;1;1;1;1;15;2;7;5;6;33;100;2000;2020|]) in
  let stream = idx |> Seq.concat_map (fun i -> arr.(i) |> List.to_seq |> Seq.cycle |> Seq.take (Toolkit.choice 10 [10000;20000;])) in
  stream 

  

let one_pole_filter ~alpha (seq : float Seq.t) : float Seq.t =
  let rec aux prev_value seq () =
    match seq () with
    | Seq.Nil -> Seq.Nil
    | Seq.Cons (x, xs) ->
        let y = (alpha *. x) +. ((1.0 -. alpha) *. prev_value) in
        Seq.Cons (y, aux y xs)
  in
  match seq () with
  | Seq.Nil -> Seq.empty
  | Seq.Cons (first, rest) -> aux first rest

let percussive_envelope ~decay seq () =
  let rec aux prev_value seq () =
    match seq () with
    | Seq.Nil -> Seq.Nil
    | Seq.Cons (x, xs) ->
        let y = decay *. prev_value in
        if y <= 0.000001 then Seq.Nil (* Stop when the envelope is very small *)
        else Seq.Cons (y *. x, aux y xs)
  in
  aux 1.0 seq |> Seq.take 1000000 (* Start with an impulse *)

let sequence () = 
  let open Cisp in
  line_chain (ch [|1.;4000.|]) (st 40) |> Seq.concat_map (fun r -> (percussive_envelope ~decay:(1.0 -. (1.0 /. r))) (white) ())

let tupToList (x,y) = [x;y]

let panned = 
  Cisp.stereo_pan_fold (Cisp.uzi 22 snippers |> List.of_seq) |> tupToList

let _ =
  Jack.playSeqs 0 Process.sample_rate (panned)
