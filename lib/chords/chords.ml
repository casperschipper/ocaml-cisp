(* Constants for the roughness function *)
let alpha = 1.25

let beta = 0.25

(* Roughness function R(delta_f) based on the difference between two frequencies *)
let roughness delta_f =
  let exp1 = exp (-.alpha *. delta_f) in
  let exp2 = exp (-.beta *. (delta_f ** 2.0)) in
  exp1 *. exp2

let calculate_roughness frequencies =
  let rec pairwise_roughness freqs acc =
    match freqs with
    | []
     |[_] ->
        acc (* Base case: if there's 0 or 1 frequency left, we're done *)
    | f1 :: rest ->
        (* Sum the roughness for pairs with the first element and each of the rest *)
        let acc =
          List.fold_left
            (fun acc f2 -> acc +. roughness (abs_float (f2 -. f1)))
            acc rest
        in
        pairwise_roughness rest acc
  in
  pairwise_roughness frequencies 0.0

(** just a walk with n steps *)
let rec walk_steps init n f =
  if n <= 0 then Seq.empty
  else fun () ->
    let next = f init in
    Seq.Cons (init, walk_steps next (n - 1) f)

let walk_states init n f =
  Infseq.unfold f init |> Infseq.to_seq |> Seq.take n




