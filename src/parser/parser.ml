(* super tiny parser ! *)

type ('a,'b) pstep = 
  | Good of 'a * char Seq.t (* the happy path *)
  | Problem of 'b * char Seq.t (* the unhappy *)

type ('a,'b) parser = Parser of (char Seq.t -> ('a,'b) pstep)

let succeed a = Parser (fun _ -> Good (a,Seq.empty))



let parse (Parser p) s = p s

let getParsed parseResult =
  match parseResult with
  | Good (a,_) -> Some a
  | Problem (_,_) -> None

let mapProblem parsed f =
  match parsed with
  | Good (a,_) -> a
  | Problem (prob,chars) -> (f (prob,chars) ) 

let getRemainChars parseResult =
  match parseResult () with
  | Good (_,chars) -> Some chars
  | Problem (_,_) -> None





(**
 
 *)

let bind p f =
  let step result =
    match result with
    | Good (a,chars) -> parse (f a) chars 
    | Problem (prob,chars) -> Problem (prob,chars)
  in
  Parser (fun s -> s |> parse p |> step)

let ( >>= ) = bind

let ( >> ) m k = m >>= fun _ -> k

let unit a = succeed a

let pure = unit

let return = unit

let item =
  Parser
    (fun charSq ->
      match charSq () with
      | Seq.Nil -> failwith "reached the end"
      | Seq.Cons (c, cs) -> Good (c,cs) )

(*
           
let bool char =
  Parser (fun char -> match char with 'x' -> true | '.' -> false) *)

let fmap f p =
  Parser (fun s -> 
      let result = parse p s in
      match result with
      | Good (a,rest) -> Good (f a,rest)
      | Problem (prob,rest) -> Problem (prob,rest)
    )

let ( <$> ) = fmap

(*   (Parser cs1) <*> (Parser cs2) = Parser (\s -> [(f a, s2) | (f, s1) <- cs1 s, (a, s2) <- cs2 s1]) *)

let product l1 l2 =
  List.rev
    (List.fold_left
       (fun x a -> List.fold_left (fun y b -> b :: a :: y) x l2)
       [] l1 )

let applyP fP xP = fP >>= fun f -> xP >>= fun x -> return (f x)

let applicative = applyP

let ( <*> ) = applyP

let andThen pa pb = pa >>= fun resA -> pb >>= fun resB -> return (resA, resB)

let failure problem = Parser (fun _ -> Problem (problem,Seq.empty) )

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
  match a () with
  | Seq.Nil -> b ()
  | Seq.Cons (h, ls) -> Seq.Cons (h, append ls b)
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
      match res with 
        | Good (_, _) -> res
        | Problem (_,_) -> parse q s)
      

let ( <|> ) = option

(*
let mzero = failure

let mplus = combine

let empty = mzero
 *)

let liftA2 f p1 p2 = f <$> p1 <*> p2

let oneOfParsers parsers =
  let rec oneOfHelp s0 parsers =
      match parsers with
        | [] ->
          Problem ("sorry none of the parsers fitted",s0)
        | p :: remainingParsers ->
          match parse p s0 with
            | Good (a,rest) -> Good (a,rest)
            | Problem _ -> oneOfHelp s0 remainingParsers
  in
  Parser (fun s -> oneOfHelp s parsers)


type ('state, 'a) step = Loop of 'state | Done of 'a

let rec loopHelp state callback s0 =
  let myParser = 
    callback state 
  in
  let result = parse myParser s0 in
  match result with 
  | Good(step,s1) -> 
      begin
      match step with
      | Loop newState ->
          loopHelp newState callback s1
      | Done result ->
          Good (result,s1)
      end
  | Problem(p,rest) -> Problem (p,rest)
        
let loop state callback = Parser (fun chrs -> 
  loopHelp state callback chrs)


let parseZeroOrMore p =
  let callback state =
    oneOfParsers [
      p |> fmap (fun item -> Loop (item::state))
      ;succeed () |> fmap (fun () -> (Done (List.rev state)))
    ]
  in
  loop [] callback

let many p = parseZeroOrMore p

let some p = List.cons <$> p <*> many p

let satisfy predicate expecting =
  item >>= fun c -> if predicate c then unit c else failure ("expected a " ^ expecting)

let flip f x y = (f y) x

let elem x xs = List.exists (fun x' -> x' == x) xs

let explode = String.to_seq

let explode_lst str = str |> explode |> List.of_seq

let one_of s expecting = satisfy (flip elem s) expecting

let one_of_string str expecting = str |> explode_lst |> flip one_of expecting

let chainl1 p op =
  let rec rest a = op >>= (fun f -> p >>= fun b -> rest (f a b)) <|> return a in
  p >>= fun a -> rest a

let opt_int_of_string str =
  try Option.Some (int_of_string str) with Failure _ -> None

let int_of_char_seq charLst =
  let str = charLst |> List.to_seq |> String.of_seq in
  opt_int_of_string str |> Option.value ~default:0

let char c  = satisfy (fun ch -> ch == c) (c |> Seq.return |> String.of_seq)

let is_digit = function '0' .. '9' -> true | _ -> false

let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false

let digitP = satisfy is_digit "digit"

let natural = int_of_char_seq <$> some (satisfy is_digit "digit")

let spaces = many (one_of_string " \n\r" "expected space")

let chainl p op a = chainl1 p op <|> return a

let chainl1 p op =
  let rec rest a = op >>= fun f -> p >>= fun b -> rest (f a b) <|> return a in
  p >>= fun a -> rest a


let between openSymbol closeSymbol p =
  openSymbol >> p >>= fun x -> closeSymbol >> return x




let float = 
  let divideUntilRight n =
    let rec aux x = 
      if x < 1.0 then
        x
      else
        x /. 10.0 |> aux
    in
    n |> float_of_int |> aux
  in
  let finishInt firstDigits =
    natural >>= fun lastDigits -> (float_of_int firstDigits) +. (divideUntilRight lastDigits) |> return
  in
  natural >>= 
  fun firstDigits -> 
    char '.' >>
    (oneOfParsers [
       finishInt firstDigits
      ;return (float_of_int firstDigits)])
  
  
let list_end n1 =
  loop [n1] 
    (fun state -> 
      spaces 
      >> oneOfParsers 
        [ natural |> fmap (fun num -> Loop (num::state))
        ; char ')' |> fmap (fun _ -> Done (List.rev state)) ])

let natural_number_list =
  char '('
  >> spaces  
  >> oneOfParsers [ 
    natural >>= list_end
    ;char ')' |> fmap (fun _ -> []) 
  ]
  

(* what is a list? 

() -> empty list
(seq) -> seq []
(seq 1) -> seq [1]
(seq 1,2) -> seq (2::[1] |> reverse) 
*)

let sepBy1 p sep = p >>= fun x -> many (sep >> p) >>= fun xs -> return (x :: xs)

let sepBy p sep = sepBy1 p sep <|> return []

let slist = between (char '(') (char ')') (sepBy natural spaces)

let singletonP = (fun x -> [x]) <$> natural

let string str =
  let rec aux sq =
    match sq () with
    | Seq.Nil -> return []
    | Seq.Cons (chr, rest) -> List.cons <$> char chr <*> aux rest
  in
  aux (String.to_seq str) |> fmap (fun charSeq -> charSeq |> List.to_seq |> String.of_seq)

let token p = p >>= fun a -> spaces >> return a

let reserved s = token (string s)

let parens m = reserved "(" >> m >>= fun n -> reserved ")" >> return n

