type charClass = Alpha | Digit | AlphaNum | Special
val class_as_string : charClass -> string
type 'p problem =
    EndOfString
  | Expecting of string
  | WrongCharClass of charClass * charClass
  | NoMatch
  | ExpectedEnd
  | YourProblem of 'p
val problem_to_string : ('a -> string) -> 'a problem -> string
type 'p non_empty = NonEmpty of 'p problem * 'p problem list
val problem : 'a problem -> 'a non_empty
val appendProblem : 'a problem -> 'a non_empty -> 'a non_empty
type ('a, 'problem) pstep =
    Good of 'a * char Seq.t
  | Problem of 'problem * char Seq.t
type ('a, 'b) parser = Parser of (char Seq.t -> ('a, 'b) pstep)
val succeed : 'a -> ('a, 'b) parser
val fail_with : 'a -> ('b, 'a problem) parser
val parse : ('a, 'b) parser -> char Seq.t -> ('a, 'b) pstep
val lazyp : (unit -> ('a, 'b) parser) -> ('a, 'b) parser
val getParsed : ('a, 'b) pstep -> 'a option
val mapProblem : ('a, 'b) pstep -> ('b * char Seq.t -> 'a) -> 'a
val getRemainChars : ('a, 'b) pstep -> char Seq.t option
val bind : ('a, 'b) parser -> ('a -> ('c, 'b) parser) -> ('c, 'b) parser
val ( >>= ) : ('a, 'b) parser -> ('a -> ('c, 'b) parser) -> ('c, 'b) parser
val ( >> ) : ('a, 'b) parser -> ('c, 'b) parser -> ('c, 'b) parser
val unit : 'a -> ('a, 'b) parser
val pure : 'a -> ('a, 'b) parser
val return : 'a -> ('a, 'b) parser
val item : (char, 'a problem) parser
val fmap : ('a -> 'b) -> ('a, 'c) parser -> ('b, 'c) parser
val ( <$> ) : ('a -> 'b) -> ('a, 'c) parser -> ('b, 'c) parser
val product : 'a list -> 'a list -> 'a list
val applyP : ('a -> 'b, 'c) parser -> ('a, 'c) parser -> ('b, 'c) parser
val applicative : ('a -> 'b, 'c) parser -> ('a, 'c) parser -> ('b, 'c) parser
val ( <*> ) : ('a -> 'b, 'c) parser -> ('a, 'c) parser -> ('b, 'c) parser
val andThen : ('a -> ('b, 'c) parser) -> ('a, 'c) parser -> ('b, 'c) parser
val failure : 'a -> ('b, 'a) parser
val append : 'a Seq.t -> (unit -> 'a Seq.node) -> 'a Seq.t
val option : ('a, 'b) parser -> ('a, 'b) parser -> ('a, 'b) parser
val ( <|> ) : ('a, 'b) parser -> ('a, 'b) parser -> ('a, 'b) parser
val liftA2 :
  ('a -> 'b -> 'c) -> ('a, 'd) parser -> ('b, 'd) parser -> ('c, 'd) parser
val one_of_parsers : ('a, 'b) parser list -> ('a, 'c problem) parser
type ('state, 'a) step = Loop of 'state | Done of 'a
val loop : 'a -> ('a -> (('a, 'b) step, 'c) parser) -> ('b, 'c) parser
val zero_or_more : ('a, 'b) parser -> ('a list, 'c problem) parser
val many : ('a, 'b) parser -> ('a list, 'c problem) parser
val lookahead : ('a, 'b) parser -> ('a, 'b) parser
val try_parser : ('a, 'b) parser -> ('a, 'b) parser
val some : ('a, 'b problem) parser -> ('a list, 'b problem) parser
val one_or_more : ('a, 'b problem) parser -> ('a list, 'b problem) parser
val satisfy : (char -> bool) -> string -> (char, 'a problem) parser
val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
val elem : 'a -> 'a list -> bool
val explode : String.t -> char Seq.t
val parse_str : ('a, 'b) parser -> String.t -> ('a, 'b) pstep
val explode_lst : String.t -> char list
val one_of : char list -> (char, 'a problem) parser
val one_of_str : String.t -> (char, 'a problem) parser
val one_of_string : String.t -> (char, 'a problem) parser
val opt_int_of_string : string -> int Option.t
val int_of_char_seq : char list -> int
val char : char -> (char, 'a problem) parser
val is_digit : char -> bool
val is_alpha : char -> bool
val not_space : char -> bool
val is_space : char -> bool
val digitP : (char, 'a problem) parser
val natural : (int, 'a problem) parser
val spaces : (char list, 'a problem) parser
val chainl :
  ('a, 'b) parser -> ('a -> 'a -> 'a, 'b) parser -> 'a -> ('a, 'b) parser
val chainl1 :
  ('a, 'b) parser -> ('a -> 'a -> 'a, 'b) parser -> ('a, 'b) parser
val between :
  ('a, 'b) parser -> ('c, 'b) parser -> ('d, 'b) parser -> ('d, 'b) parser
type number = Float of float | Integer of int
val number_to_string : number -> string
val number_to_float : number -> float
val number : (number, 'a problem) parser
val list_end : int -> (int list, 'a problem) parser
val natural_number_list : (int list, 'a problem) parser
val sepBy1 :
  ('a, 'b problem) parser ->
  ('c, 'b problem) parser -> ('a list, 'b problem) parser
val sepBy :
  ('a, 'b problem) parser ->
  ('c, 'b problem) parser -> ('a list, 'b problem) parser
val slist : (int list, 'a problem) parser
val singletonP : (int list, 'a problem) parser
val string : String.t -> (String.t, 'a problem) parser
val token : ('a, 'b problem) parser -> ('a, 'b problem) parser
val reserved : String.t -> (String.t, 'a problem) parser
val reached_end : (unit, 'a problem) parser
val of_char_list : char list -> String.t
val parse_while : (char -> bool) -> (char list, 'a problem) parser
val parens : ('a, 'b problem) parser -> ('a, 'b problem) parser
val string2sumtype : (string * 'a) list -> ('a, 'b problem) parser
val test : unit -> String.t list option
