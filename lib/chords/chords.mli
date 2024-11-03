val roughness : float -> float
val calculate_roughness : float list -> float
val walk_steps : 'a -> int -> ('a -> 'a) -> 'a Seq.t
val walk_states : 'a -> int -> ('a -> 'b * 'a) -> 'b Seq.t
