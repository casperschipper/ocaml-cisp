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
 
  let wrap low high x =
    let range = low - high |> abs in
    let modded = 
      match range with
      | 0 -> low
      | non_zero_range -> 
        (x - low) mod non_zero_range in
    if modded < 0 then high + x else low + x