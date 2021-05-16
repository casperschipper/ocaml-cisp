(* infinite seq type without Nil *)
type 'a inf_node = InfCons of 'a * 'a t
and 'a t = unit -> 'a inf_node

val countFrom : int -> int t
val map : ('a -> 'b) -> 'a t -> 'b t
val repeat : 'a -> 'a t
val head : (unit -> 'a inf_node) -> 'a
val tail : (unit -> 'a inf_node) -> 'a t
val seq : 'a list -> 'a t

(* use sparingly! *)
val toSeq : 'a t -> 'a Seq.t

(* cut a finite part *)
val take : int -> 'a t -> 'a Seq.t

(* concat a infinite number of finite seqs, as an infintie sq *)
val concatSq : 'a Seq.t t -> 'a t
val cycleSq : 'a Seq.t -> 'a t
val unfold : ('a -> 'b * 'a) -> 'a -> 'b t
val uncons : (unit -> 'a inf_node) -> 'a * 'a t
val recursive : 'a t -> 'b -> ('a -> 'b -> 'b) -> ('b -> 'c) -> 'c t
val recursive1 : 'a t -> 'b -> ('a -> 'b -> 'b) -> ('b -> 'c) -> 'c t
val walki : int -> int t -> int t
val walk : float -> float t -> float t
val sometimes : 'a -> 'a -> int -> 'a t
val zip : 'a t -> 'b t -> ('a * 'b) t
val zipWith : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
val applySq : ('a -> 'b) t -> 'a t -> 'b t
