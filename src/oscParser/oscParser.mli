(* ocamlc -I /Users/casperschipper/.opam/4.11.1+flambda/lib/lo -i oscParser.ml >> oscParse2.mli *)

type data = Lo.Message.data

module R = Result

type 'a t = OscParser of (data Seq.t -> ('a * data Seq.t, string) Result.t)

val ofData :
     [> `Double of float
     | `False
     | `Float of float
     | `Int32 of int
     | `String of string
     | `True ]
  -> string

val run : 'a t -> data Seq.t -> ('a * data Seq.t, string) Result.t

val return : 'a -> 'a t

val fmap : ('a -> 'b) -> 'a t -> 'b t

val bind : 'a t -> ('a -> 'b t) -> 'b t

val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

val ( >> ) : 'a t -> 'b t -> 'b t

val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t

val failure : string -> 'a t

val item : data t

val satisfy : (data -> bool) -> string -> data t

val option : 'a t -> 'a t -> 'a t

val isString : 'a -> [> `String of 'a] -> bool

val float : float t

val int : int t

val flt_par : string -> float t

val int_par : string -> int t

val str_par : string -> string t

val bool_par : string -> bool t
