(* super tiny parser ! *)
open Format

type 'a parser = Parser of (char Seq.t -> ('a * char Seq.t) Seq.t)

let parse (Parser p) s = p s

let parse_string_to_list (Parser p) s = p (String.to_seq s) |> List.of_seq

(**
 
 *)

let bind p f =
  let step = Seq.flat_map (fun (a, ss) -> parse (f a) ss) in
  Parser (fun s -> step (parse p s))

let ( >>= ) = bind

let ( >> ) m k = m >>= fun _ -> k

let unit a = Parser (fun s -> Seq.return (a, s))

let pure = unit

let return = unit

let item =
  Parser
    (fun charSq ->
      match charSq () with
      | Seq.Nil -> Seq.empty
      | Seq.Cons (c, cs) -> Seq.return (c, cs))

(*
           
let bool char =
  Parser (fun char -> match char with 'x' -> true | '.' -> false) *)

let fmap f (Parser p) =
  let mapFst f (a, s) = (f a, s) in
  Parser (fun s -> Seq.map (mapFst f) (p s))

let ( <$> ) = fmap

(*   (Parser cs1) <*> (Parser cs2) = Parser (\s -> [(f a, s2) | (f, s1) <- cs1 s, (a, s2) <- cs2 s1]) *)

let product l1 l2 =
  List.rev
    (List.fold_left
       (fun x a -> List.fold_left (fun y b -> b :: a :: y) x l2)
       [] l1)

let applyP fP xP = bind fP (fun f -> bind xP (fun x -> return (f x)))

let applicative = applyP

let ( <*> ) = applyP

let andThen pa pb = pa >>= fun resA -> pb >>= fun resB -> return (resA, resB)

let failure = Parser (fun _ -> Seq.empty)

(* combine
   combine p q = Parser (\s -> parse p s ++ parse q s)
 *)

(*
let rec concat sq () =
  let open Seq in
  match sq () with
  | Nil -> Nil
  | Cons (h, ls) -> (
    match h () with
    | Cons (h', ls') ->
        let newtail () = Cons (ls', ls) in
        Cons (h', concat newtail)
    | Nil -> concat ls () )*)

let rec append a b () =
  let open Seq in
  match a () with Seq.Nil -> b () | Cons (h, ls) -> Cons (h, append ls b)

let combine p q =
  Parser
    (fun s ->
      let res1 = parse p s in
      let res2 = parse q s in
      append res1 res2)

(*
   option :: Parser a -> Parser a -> Parser a
   option  p q = Parser $ \s ->
     case parse p s of
       []     -> parse q s
       res    -> res *)

let option p q =
  Parser
    (fun s ->
      let res = parse p s in
      match res () with Seq.Nil -> parse q s | _ -> res)

let ( <|> ) = option

let mzero = failure

let mplus = combine

let empty = mzero

let ( <*> ) = applyP

let liftA2 f p1 p2 = f <$> p1 <*> p2

(** ambiguous parsers will be ignored *)
let rec parseZeroOrMore p input =
  let firstResult = parse p input in
  match firstResult () with
  | Seq.Nil -> ([], input)
  | Seq.Cons ((res, remainInput), _) ->
      let subsequentValues, remainingInput = parseZeroOrMore p remainInput in
      let values = List.cons res subsequentValues in
      (values, remainingInput)

let many p = Parser (fun input -> Seq.return (parseZeroOrMore p input))

let some p = List.cons <$> p <*> many p

let satisfy predicate =
  item >>= fun c -> if predicate c then unit c else failure

let flip f x y = (f y) x

let elem x xs = List.exists (fun x' -> x' == x) xs

let explode = String.to_seq

let explode_lst str = str |> explode |> List.of_seq

let one_of s = satisfy (flip elem s)

let one_of_string str = explode_lst str |> one_of

let chainl1 p op =
  let rec rest a = op >>= (fun f -> p >>= fun b -> rest (f a b)) <|> return a in
  p >>= fun a -> rest a

let chainl p op a = chainl1 p op <|> return a

let opt_int_of_string str =
  try Option.Some (int_of_string str) with Failure _ -> None

let int_of_char_seq charLst =
  let str = charLst |> List.to_seq |> String.of_seq in
  opt_int_of_string str |> Option.value ~default:0

let char c = satisfy (fun ch -> ch == c)

let is_digit = function '0' .. '9' -> true | _ -> false

let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false

let digitP = satisfy is_digit

let natural = int_of_char_seq <$> some (satisfy is_digit)

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

let rec string str =
  let rec aux sq =
    match sq () with
    | Seq.Nil -> return []
    | Seq.Cons (chr, rest) -> List.cons <$> char chr <*> aux rest
  in
  aux (String.to_seq str)

let spaces = many (one_of_string " \n\r")

let token p = p >>= fun a -> spaces >> return a

let reserved s = token (string s)

let parens m = reserved "(" >> m >>= fun n -> reserved ")" >> return n

let test = "x..x."

let test = "1..2 1!2 1 2 3 "

let range a b =
  let op = if a > b then fun x -> x - 1 else ( + ) 1 in
  let rec aux a b = if a = b then [a] else a :: aux (op a) b in
  aux a b

let rangeP =
  natural >>= fun a -> string ".." >> natural >>= fun b -> return (range a b)

let chainl p op a = chainl1 p op <|> return a

(**
   how to do arrays !?
 let lijstP = chainl natural spaces

https://github.com/elm/parser/blob/02839df10e462d8423c91917271f4b6f8d2f284d/src/Parser/Advanced.elm#L393

 *)

let chainl1 p op =
  let rec rest a = op >>= fun f -> p >>= fun b -> rest (f a b) <|> return a in
  p >>= fun a -> rest a

(**
   program = int | expr
   expr = int <|> range <|> announcement
   range = int (symbol "..") int
   
    
 *)
