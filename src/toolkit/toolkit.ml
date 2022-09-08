(* here should go, wrap rvi rvfi etc...contents *)

let rvfi low high =
  let range = abs_float (low -. high) in
  let offset = min low high in
  Random.float range +. offset

let rvi low high =
  let range = abs (low - high) in
  let offset = min low high in
  let rnd = match range with
  | 0 -> 0
  | x -> Random.int x 
  in
  rnd + offset

let modBy y x =
  match y with
  | 0 -> y (* safety first ! *)
  | nonZeroY ->
      let result = x mod nonZeroY in
      if result >= 0 then result else result + y

let modByf y x =
  match y with
  | 0.0 -> y
  | nonZeroY -> 
    let result = mod_float x nonZeroY in
    if result >= 0.0 then result else result +. y

let wrapf low high x =
  let l = min low high in
  let r = abs_float (high -. low) in
  l +. ((x -. l) |> modByf r)

let wrap low high x =
  let l = min low high in
  let r = abs (high - low) in
  l + ((x - l) |> modBy r)

let%test "modBy mod x" =
  let input = [0;1;2;3;4;5;6] in
  let expect = [0;1;2;0;1;2;0] in 
  List.equal Int.equal (input |> List.map (modBy 3)) expect

let%test "wrap" = 
  let input = [0;1;2;3;4;5] in    
  let expect = [0;1;2;0;1;2] in
  List.equal Int.equal (input |> List.map (wrap 0 3)) expect

let%test "wrap negative" = 
  let input = [-1;0;1;2;3;4] in
  let expect = [3;0;1;2;3;0] in
  List.equal Int.equal (input |> List.map (wrap 0 4)) expect

let%test "wrap higher" = 
  let input = [7;8;9;10;11;12;13] in
  let expect = [9;8;9;8;9;8;9] in
  List.equal Int.equal (input |> List.map (wrap 8 10)) expect

let%test "wrap 0" =
  let input = [0;1;2;3;4] in
  let expect = [0;0;0;0;0] in
  List.equal Int.equal (input |> List.map (wrap 0 0)) expect


      
