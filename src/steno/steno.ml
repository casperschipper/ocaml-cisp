(* super tiny parser ! *)

type 'a parser = Parser of (char list -> ('a * char list) list)

let parse (Parser p) s = p s

let test = "x.."

let expect = [true; false; false]

let bind p f =
  let step = List.concat_map (fun (a, ss) -> parse (f a) ss) in
  Parser (fun s -> step (parse p s))

let ( >>= ) p f = bind

let unit a = Parser (fun s -> [(a, s)])

let pure = unit

let return = unit

let fmap f (Parser p) =
  let mapFst f (a, s) = (f a, s) in
  Parser (fun s -> List.map (mapFst f) (p s))

let ( <$> ) = fmap

(*   (Parser cs1) <*> (Parser cs2) = Parser (\s -> [(f a, s2) | (f, s1) <- cs1 s, (a, s2) <- cs2 s1]) *)

(* we need a cartesian product *)

let product l1 l2 =
  List.rev
    (List.fold_left
       (fun x a -> List.fold_left (fun y b -> b :: a :: y) x l2)
       [] l1)

let applicative (Parser a) (Parser b) =
  Parser
    (fun s ->
      let parsedWithA = a s in
      let parsedWithB = b s in
      product parsedWithA parsedWithB)

let failure = Parser (fun cs -> [])

(* combine
   combine p q = Parser (\s -> parse p s ++ parse q s)
 *)

let combine p q = Parser (fun s -> parse p s @ parse q s)

(*
   option :: Parser a -> Parser a -> Parser a
   option  p q = Parser $ \s ->
     case parse p s of
       []     -> parse q s
       res    -> res *)

let option p q =
  Parser (fun s -> match parse p s with [] -> parse q s | res -> res)

let ( <|> ) = option

let mzero = failure

let mplus = combine

let empty = mzero

(*
                -- | One or more.
some :: f a -> f [a]
some v = some_v
  where
    many_v = some_v <|> pure []
    some_v = (:) <$> v <*> many_v

-- | Zero or more.
many :: f a -> f [a]
many v = many_v
  where
    many_v = some_v <|> pure []
    some_v = (:) <$> v <*> many_v

 *)

let ( <*> ) = applicative

let some v =
  let rec some_v v = applicative (fmap mplus v) (some_v v <|> empty) in
  some_v v
