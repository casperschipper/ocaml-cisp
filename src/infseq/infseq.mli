type 'a t =
    unit -> 'a inf_node
and 'a inf_node =
  InfCons of 'a * 'a t
  
val countFrom : int -> int t
val cycleSq : 'a Seq.t -> 'a t
val take : int -> 'a t -> 'a Seq.t
val toSeq : 'a t -> 'a Seq.t
val concatSq : 'a Seq.t t -> 'a t
val unfold : ('a -> 'b * 'a) -> 'a -> 'b t
val uncons : 'a t -> 'a * 'a t
val map : ('a -> 'b) -> 'a t -> 'b t
val repeat : 'a -> 'a t
val recursive : 'a t -> 'b -> ('a -> 'b -> 'b) -> ('b -> 'c) -> 'c t
val recursive1 : 'a t -> 'b -> ('a -> 'b -> 'b) -> ('b -> 'c) -> 'c t
