open Parser

(** start of actual steno program *)

let pattElem = char 'x'

type pattern = Rest | Emphasis | Normal | FadeIn | FadeOut | Never

let pat_of_char = function
  | 'x' -> Emphasis
  | '.' -> Rest
  | '-' -> Normal
  | '/' -> FadeIn
  | '\\' -> FadeOut
  | _ -> Never

let bool_of_char = function 'x' -> true | '.' -> false | _ -> false

let stenoMapper voca smap = many (smap <$> one_of (explode voca |> List.of_seq))

let patternP = many (pat_of_char <$> one_of (explode "x.-/\\" |> List.of_seq))

let boolmask = many (bool_of_char <$> one_of (explode "x." |> List.of_seq))

type sect =
  | Range of int * int
  | Value of int
  | Repeat of int * int
  | Pattern of int

let range a b =
  let op = if a > b then fun x -> x - 1 else ( + ) 1 in
  let rec aux a b = if a = b then [a] else a :: aux (op a) b in
  aux a b

let repeats x n =
  let rec aux x n = match n with 0 -> [] | n' -> x :: aux x (n' - 1) in
  if n < 1 then [] else aux x n

let rangeFromThenTo a b c =
  let diff = b - a in
  let finished a c =
    match (a < b, b < c) with
    | true, true -> fun a c -> a > c (* 1,2 3 *)
    | true, false -> fun _ _ -> true (* 1,3,2 *)
    | false, false -> fun a c -> a < c (* 3 2 1 *)
    | false, true -> fun _ _ -> true
  in
  let rec aux a c diff finished =
    if finished a c then [] else a :: aux (a + diff) c diff finished
  in
  aux a c diff (finished a c)

let rangeP =
  natural >>= fun a -> string ".." >> natural >>= fun b -> return (range a b)

let repeatsP =
  natural >>= fun a -> char '!' >> natural >>= fun b -> return (repeats a b)

let rangeFromThenToP =
  natural
  >>= fun a ->
  char ',' >> natural
  >>= fun b ->
  string ".." >> natural >>= fun c -> return (rangeFromThenTo a b c)

let stenoP =
  let shortHand = rangeP <|> repeatsP <|> rangeFromThenToP <|> singletonP in
  List.concat <$> sepBy shortHand (char ' ')

(**
   program = int | expr
   expr = int <|> range <|> announcement
   range = int (symbol "..") int
   
    
 *)
