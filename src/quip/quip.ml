open Parser

let assert_equal label a b = if a = b then print_string "all ok" else print_string ("failed: " ^ label ^ " <<<should be>>> ")

(* bunch of standard symbols *)
type generator =
   | Seq
   | Rv
   | Choice
   | Line
   | Symbol of string




let fromSymbol s = 
  match s with
  | "seq" -> Seq
  | symbol -> Symbol symbol
  


 
    
(* (2 3 4 5 6) *)
(* (/ 1 2 3 4) *)

type expr =
  | Int of int
  | Float of float 
  | Symbol of string
  | List of expr list

  (* 
  
  (seq 11 12 13) -> seq int int int

  (seq 11 (rv 1 10) 13) -> (seq 11 (st 11) (rv 1 10) 13)

  (rv 1 10) -> 

  (seq (rv 1 10) 1 4)

  (count 10) 
  
  

  
  *)


(*
let rec quip_program = 
  one_of_parsers [
    Parser.float |> fmap (fun x -> Float x)
    ; Parser.natural |> fmap (fun x -> Int x)
    ; Parser. |> fmap (fun x -> String )
  ]
  *)

  (* parsing = computing ! *)

 