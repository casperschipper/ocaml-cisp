open Parser

let assert_equal label a b = if a = b then print_string "all ok" else print_string ("failed: " ^ label ^ " <<<should be>>> ")

type generator =
   | Seq
   | Rv
   | Choice
   | Line
   | Symbol of string



let generatorP = 
    one_of_parsers [ 
      Parser.string "seq"
      ;Parser.string "rv"
      ;Parser.string "choice"
      ;Parser.string "line" 
    ]
    |> fmap (fun str -> 
      match str with 
      | "seq" -> Seq
      | "rv" -> Rv
      | "choice" -> Choice
      | "line" -> Line
      | str -> Symbol str)

 
    
(* (2 3 4 5 6) *)
(* (/ 1 2 3 4) *)

type expr =
  | Int of int
  | Float of float 
  | Symbol of 
  | List of expr list


let rec quip = 
  one_of_parsers [
    Parser.float |> fmap (fun x -> Float x)
    ; Parser.natural |> fmap (fun x -> Int x)
    ; generatorP |> 
    ; 
  ]

 