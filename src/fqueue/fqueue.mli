type 'a t

val empty : 'a t

val is_empty : 'a t -> bool

val enqueue : 'a -> 'a t -> 'a t

val peek : 'a t -> 'a option

val dequeue : 'a t -> 'a t

val to_list : 'a t -> 'a list
