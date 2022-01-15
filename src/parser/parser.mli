type 'a parser = Parser of (char Seq.t -> ('a * char Seq.t) Seq.t)
val parse : 'a parser -> char Seq.t -> ('a * char Seq.t) Seq.t
val getParsed : (unit -> ('a * 'b) Seq.node) -> 'a option
val getRemainChars : (unit -> ('a * char Seq.t) Seq.node) -> char Seq.t
val parse_string : 'a list parser -> String.t -> 'a list
val bind : 'a parser -> ('a -> 'b parser) -> 'b parser
val ( >>= ) : 'a parser -> ('a -> 'b parser) -> 'b parser
val ( >> ) : 'a parser -> 'b parser -> 'b parser
val unit : 'a -> 'a parser
val pure : 'a -> 'a parser
val return : 'a -> 'a parser
val item : char parser
val fmap : ('a -> 'b) -> 'a parser -> 'b parser
val ( <$> ) : ('a -> 'b) -> 'a parser -> 'b parser
val product : 'a list -> 'a list -> 'a list
val applyP : ('a -> 'b) parser -> 'a parser -> 'b parser
val applicative : ('a -> 'b) parser -> 'a parser -> 'b parser
val ( <*> ) : ('a -> 'b) parser -> 'a parser -> 'b parser
val andThen : 'a parser -> 'b parser -> ('a * 'b) parser
val failure : 'a parser
val append : 'a Seq.t -> (unit -> 'a Seq.node) -> 'a Seq.t
val combine : 'a parser -> 'a parser -> 'a parser
val option : 'a parser -> 'a parser -> 'a parser
val ( <|> ) : 'a parser -> 'a parser -> 'a parser
val liftA2 : ('a -> 'b -> 'c) -> 'a parser -> 'b parser -> 'c parser
val parseZeroOrMore : 'a parser -> char Seq.t -> 'a list * char Seq.t
val many : 'a parser -> 'a list parser
val some : 'a parser -> 'a list parser
val satisfy : (char -> bool) -> char parser
val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
val elem : 'a -> 'a list -> bool
val explode : String.t -> char Seq.t
val explode_lst : String.t -> char list
val one_of : char list -> char parser
val one_of_string : String.t -> char parser
val opt_int_of_string : string -> int Option.t
val int_of_char_seq : char list -> int
val char : char -> char parser
val is_digit : char -> bool
val is_alpha : char -> bool
val digitP : char parser
val natural : int parser
val spaces : char list parser
val chainl : 'a parser -> ('a -> 'a -> 'a) parser -> 'a -> 'a parser
val chainl1 : 'a parser -> ('a -> 'a -> 'a) parser -> 'a parser
type ('state, 'a) step = Loop of 'state | Done of 'a
val between : 'a parser -> 'b parser -> 'c parser -> 'c parser
val loopHelp :
  'a -> ('a -> ('a, 'b) step parser) -> char Seq.t -> ('b * char Seq.t) Seq.t
val loop : 'a -> ('a -> ('a, 'b) step parser) -> 'b parser
val oneOfParsers : 'a parser list -> 'a parser
val float : float parser
val list_end : int -> int list parser
val natural_number_list : int list parser
val sepBy1 : 'a parser -> 'b parser -> 'a list parser
val sepBy : 'a parser -> 'b parser -> 'a list parser
val slist : int list parser
val singletonP : int list parser
val string : String.t -> String.t parser
val token : 'a parser -> 'a parser
val reserved : String.t -> String.t parser
val parens : 'a parser -> 'a parser
