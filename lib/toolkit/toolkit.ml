(* here should go, wrap rvi rvfi etc...contents *)

let rvfi low high =
  let range = abs_float (low -. high) in
  let offset = min low high in
  Random.float range +. offset

let rvi low high =
  let range = abs (low - high) in
  let offset = min low high in
  let rnd =
    match range with
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
  l +. (x -. l |> modByf r)

let wrap low high x =
  let l = min low high in
  let r = abs (high - low) in
  l + (x - l |> modBy r)

let shuffle d =
  let nd = List.map (fun c -> (Random.bits (), c)) d in
  let sond = List.sort compare nd in
  List.map snd sond

let choice first rest =
  let arr = first :: rest |> Array.of_list in
  let random_index = rvi 0 (Array.length arr) in
  Array.get arr random_index

let choice_arr_opt arr =
  match Array.length arr with
  | 0 -> None
  | n -> Some (Array.get arr (rvi 0 n))

let safe_get arr index =
  if Array.length arr <= index then None else Some (Array.get arr index)


let map_vertical f index arr =
  match Array.length arr with
  | 0 -> arr
  | _ ->
      Array.map
        (fun horizontal ->
          if Array.length horizontal >= index then
            f horizontal.(index)
          else horizontal )
        arr

(* In a two dimensional array, map vertically (inplace) *)

(* Function that maps over an array of arrays and updates a specific index in each sub-array *)
let update_at_index (arr : 'a array array) (index : int) (f : 'a -> 'a) : 'a array array =
  (* Map over the outer array *)
  Array.map (fun sub_array ->
    (* Check if the index is within bounds of the sub-array *)
    if Array.length sub_array > index then
      (* Update the element at the given index using the provided function *)
      let updated_sub_array = Array.copy sub_array in
      updated_sub_array.(index) <- f sub_array.(index);
      updated_sub_array
    else
      (* If the index is out of bounds, return the original sub-array *)
      sub_array
  ) arr


let flip f x y = f y x

let rec lst_take n lst =
  match (n, lst) with
  | (0, _) -> [] (* If n is 0, return an empty list *)
  | (_, []) -> [] (* If the list is empty, return an empty list *)
  | (_, x :: xs) -> x :: lst_take (n - 1) xs (* Otherwise, take the head and recurse on the tail *)


let clipf mini maxi input = 
  max mini input |> min maxi

(** Update the nth element of a list *)
let rec update_nth lst n x =
  match lst, n with
  | [], _ -> []  (* If the list is empty, return an empty list *)
  | _ :: tail, 0 -> x :: tail  (* Replace the 0th element with x *)
  | head :: tail, n when n > 0 -> head :: update_nth tail (n - 1) x  (* Recurse to the nth element *)
  | _ -> lst  (* If n is out of bounds, return the original list *)


let%test "modBy mod x" =
  let input = [0; 1; 2; 3; 4; 5; 6] in
  let expect = [0; 1; 2; 0; 1; 2; 0] in
  List.equal Int.equal (input |> List.map (modBy 3)) expect

let%test "wrap" =
  let input = [0; 1; 2; 3; 4; 5] in
  let expect = [0; 1; 2; 0; 1; 2] in
  List.equal Int.equal (input |> List.map (wrap 0 3)) expect

let%test "wrap negative" =
  let input = [-1; 0; 1; 2; 3; 4] in
  let expect = [3; 0; 1; 2; 3; 0] in
  List.equal Int.equal (input |> List.map (wrap 0 4)) expect

let%test "wrap higher" =
  let input = [7; 8; 9; 10; 11; 12; 13] in
  let expect = [9; 8; 9; 8; 9; 8; 9] in
  List.equal Int.equal (input |> List.map (wrap 8 10)) expect

let%test "wrap 0" =
  let input = [0; 1; 2; 3; 4] in
  let expect = [0; 0; 0; 0; 0] in
  List.equal Int.equal (input |> List.map (wrap 0 0)) expect
(*
   let get_safe arr idx =
     let len = Array.length arr in
     if idx >= len then
       None
     else
       Some (Array.get arr idx) *)

let fst (x, _) = x 
let sec (_, y) = y

let flatten arr = arr |> Array.fold_left Array.append [||]
