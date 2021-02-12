type 'a parser = Parser of (char list -> ('a * string) list)

val parse : 'a parser -> char list -> ('a * char list) list

val bind : 'a parser -> ('a -> 'b parser) -> 'b parser

val unit : 'a -> 'a parser

val fmap : ('a -> 'b) -> 'a parser -> 'b parser

val applicative : ('a -> 'b) parser -> 'a parser -> 'b parser

val liftA2 : ('a -> 'b -> 'c) -> 'a parser -> 'b parser -> 'c parser
