val rvfi : float -> float -> float
(**
    returns a random float
*)

val rvi : int -> int -> int
(**
    returns a random integer
*)
val wrap : int -> int -> int -> int
(**
    wrap an int between two boundaries
*)

val wrapf : float -> float -> float -> float
(** 
    wraps a float between two boundaries
*)
val shuffle : 'a List.t -> 'a List.t
val choice : 'a -> 'a List.t -> 'a
(** 
    returns a random pick from a (non empty) list
*)