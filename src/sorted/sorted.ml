(* this is a sorted list type, to insert more efficientely, useful for time based score *)
type 'a t = Sorted of 'a list

let sortedAsList (Sorted lst) = lst

let mkSorted f lst = Sorted (List.sort f lst)

(** Maybe the f is non linear, so we have to always sort afterwards *)
let map sortf f (Sorted lst) = Sorted (List.sort sortf (List.map f lst))

(** this function is not allowed to destruct the order *)
type ('a, 'b) typedFunc = Linear of ('a -> 'b)

(** this should only be used with linear functions *)
let mapLinear (Linear f) (Sorted lst) = Sorted (List.map f lst)

let emptySorted = Sorted []

let insertSorted f lst a =
  (* note, insertSorted (<) [1;2;3;4;5] 3 -returns- [1;2;3;3;4;5] *)
  let rec aux lst a =
    match lst with
    | [] -> [a]
    | h :: tl -> if f a h then a :: h :: tl else h :: aux tl a
  in
  aux lst a

let mozesSorted f sortedLst =
  let rec aux lsta (Sorted lst) =
    match lst with
    | h :: tail ->
        if f h then aux (h :: lsta) (Sorted tail)
        else (Sorted (List.rev lsta), Sorted (h :: tail))
    | [] -> (Sorted lsta, Sorted [])
  in
  aux [] sortedLst
