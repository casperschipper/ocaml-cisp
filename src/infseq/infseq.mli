type 'a inf_node = InfCons of 'a * 'a t
and 'a t = unit -> 'a inf_node
val countFrom : int -> int t
val map : ('a -> 'b) -> 'a t -> 'b t
val repeat : 'a -> 'a t
val generator : (unit -> 'a) -> 'a t
val head : (unit -> 'a inf_node) -> 'a
val tail : (unit -> 'a inf_node) -> 'a t
val toSeq : 'a t -> 'a Seq.t
val to_seq : 'a t -> 'a Seq.t
val take : int -> 'a t -> 'a Seq.t
val drop : int -> 'a t -> 'a t
val concatSq : 'a Seq.t t -> 'a t
val andMap : 'a t -> ('a -> 'b) t -> 'b t
val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
val hold : int t -> 'a t -> 'a t
val cycleSq : 'a Seq.t -> 'a t
val seq : 'a list -> 'a t
val unfold : ('a -> 'b * 'a) -> 'a -> 'b t
val uncons : (unit -> 'a inf_node) -> 'a * 'a t
val recursive : 'a t -> 'b -> ('a -> 'b -> 'b) -> ('b -> 'c) -> 'c t
val recursive1 : 'a t -> 'b -> ('a -> 'b -> 'b) -> ('b -> 'c) -> 'c t
val walki : int -> int t -> int t
val walk : float -> float t -> float t
val chunk : float t -> 'a t -> 'a Seq.t t
val zip : 'a t -> 'b t -> ('a * 'b) t
val zipWith : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
val applySq : ('a -> 'b) t -> 'a t -> 'b t
val wrap : int -> int -> int -> int
val index : 'a array -> int t -> 'a t
val index_seq : 'a t array -> int t -> 'a t
val sometimes : 'a -> 'a -> int -> 'a t
val ch_seq : 'a t array -> 'a t
val transpose : 'a t Seq.t -> 'a Seq.t t

