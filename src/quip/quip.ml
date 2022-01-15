open Parser

let assert_equal label a b = if a = b then print_string "all ok" else print_string ("failed: " ^ label ^ " <<<should be>>> ")

type generator =
   | Seq
   | Rv
   | Choice
   | Line
   | Symbol of string



let generatorP = 
    oneOfParsers [ 
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

 
    

type atom =
  | Int of int
  | Float of float 
  | List of atom list
  | Symbol of generator * atom list

(* (2 3 4 5 6) *)

let rec quip = 
  oneOfParsers [
    Parser.float |> fmap (fun x -> Float x)
    ; Parser.natural |> fmap (fun x -> Int x)
    ; generatorP |> 
    ; 
  ]

let test = 
  let result =
    parse natural_number_list ("(11 12 13 42)" |> explode) |> getParsed
  in
  assert_equal "(seq 1 2 3)" result (Some [11;12;13;42])
 