(* super tiny parser ! *)

type 'a parser = Parser of (char Seq.t -> ('a * char Seq.t) Seq.t)

let parse (Parser p) s = p s

let test = "x..x."

let test = "1..2 1!2 1 2 3 "

(**
 
 *)

let expect = [true; false; false]

let bind p f =
  let step = Seq.flat_map (fun (a, ss) -> parse (f a) ss) in
  Parser (fun s -> step (parse p s))

let ( >>= ) = bind

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

let oneOf s = satisfy (flip elem s)

let chainl1 p op =
  let rec rest a = op >>= (fun f -> p >>= fun b -> rest (f a b)) <|> return a in
  p >>= fun a -> rest a

let chainl p op a = chainl1 p op <|> return a

let explode = String.to_seq

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

let pattElem = char 'x'

type pattern = Rest | Emphasis | Normal | FadeIn | FadeOut

type sect =
  | Range of int * int
  | Value of int
  | Repeat of int * int
  | Pattern of int
