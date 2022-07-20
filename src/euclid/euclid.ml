open Cisp

type boolSieve =
  BoolSieve of bool Seq.t

let boolSieve sq =
  BoolSieve sq
    
type sieveType =
  | Union
  | Difference
  | Intersection
  | Exclusive

let divrem a b =
  let rec aux n a b =
    if b < a then
      aux (n+1) (a-b) b
    else
      (n,a)
  in
  aux 0 a b

let rec merge (result,lsta,lstb) =
  match lsta with
  | [] -> (result,[],lstb)
  | x :: xs -> 
    match lstb with
    | [] -> (result,[],lsta)
    |  y::ys -> (merge (List.append x y :: result,xs,ys))

type beat =
  | X
  | R   

let euclidRhythm a b =
  if a > b then
    []
  else
    if a = b then
      listRepeat a X
    else
      let (xs,ys) = (listRepeat a X |> List.map singleton
                    ,listRepeat (b-a) R |> List.map singleton)
      in 
      let rec compute merged remain =
        match merge ([],merged,remain) with
        | (result,[],[]) -> result
        | (result,[],rest) -> compute result rest
        | (result,_,_) -> result 
      in
      compute xs ys |> List.flatten

let printEuclid lst =
  List.fold_right (fun x acc -> match x with
                               | X -> "x" ^ acc
                               | R -> "." ^ acc) lst ""

let toBool x =
  x = X

let euclidTrigger a b =
  euclidRhythm a b |> List.map toBool |> List.to_seq |> Infseq.cycleSq

let toBinary bt =
  match bt with
  | X -> 1
  | R -> 0

let euclid01 a b =
  euclidRhythm a b |> List.map toBinary

let encode sqn =
  let f n () = Seq.Cons(1,repeat n 0) in
  sqn |> Infseq.map f |> Infseq.concatSq |> Infseq.map



(* 
[]    [[1];[1];[1];[1];[1];[1];[1]] [[0];[0];[0];[0];[0];[0];[0];[0];[0]]

[[10]] [[1];...] [[0];[0];]


[1;1;1;1;1;1;1] [0;0;0;0;0;0;0;0;0;0;0;0]

[10;10;10;10;10] [0;0;0;0]

[100;100;100;100] [10]

[100;100;100;100] []

      [10;10;10;10;10;10;10] [0;0]

      [10;10;10;0] [10;10;10;0] []


      [1;1;1;1;1] [0;0;0] 5 * 1 + 3

      [10;10;10] [1;1] 3 * 2 + 2

      [101;101] [10] 2 * 3 + 1

      [101;101;10]

    [1;1;1;1;1;1] [0;0]

      [1;1;0] [1;1;0] [1;1]
   [1;1;0;1] [1;1;0;1] []

    [1;1;1;1;1] [0;0;0]

      [10;10;10] [1;1]

      [101;101] [10]

      [101;101;10] []


[1;1;1;1;1;1;1] [

   
5 / 8

5 + 3
3 + 2
2 + 1

*)

let sieve sieveCombinator (BoolSieve a) (BoolSieve b) =
  match sieveCombinator with
  | Union -> zipWith (||) a b
  | Difference -> zipWith (<>) a b
  | Intersection -> zipWith (&&) a b
  | Exclusive -> zipWith (fun x y -> match (x,y) with (true,false) -> true | (_,_) -> false) a b
