type 'a parser = Parser of (char list -> ('a * string) list)

val parse : 'a parser -> char Seq.t -> ('a * char Seq.t) Seq.t

val item : char parser

val bind : 'a parser -> ('a -> 'b parser) -> 'b parser

val unit : 'a -> 'a parser

val pure : 'a -> 'a parser

val return : 'a -> 'a parser

val fmap : ('a -> 'b) -> 'a parser -> 'b parser

val applicative : ('a -> 'b) parser -> 'a parser -> 'b parser

val liftA2 : ('a -> 'b -> 'c) -> 'a parser -> 'b parser -> 'c parser

val many : 'a parser -> 'a list parser

val some : 'a parser -> 'a list parser

val satisfy : ('a -> 'b) -> 'a parser

val chainl : 'a parser -> ('a -> 'a -> 'a) parser -> 'a parser

val chainl1 : 'a parser -> ('a -> 'a -> 'a) parser -> 'a parser
