type 'a t =
  { cache : ('a Array.t) Option.t
  ; input : 'a Seq.t }

val eval : 
