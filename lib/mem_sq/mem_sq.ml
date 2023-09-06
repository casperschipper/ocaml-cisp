type 'a t =
  { cache : ('a Array.t) Option.t
  ; input : 'a Seq.t }

let seqify =
  Array.to_seq

let chachingSeq =
  let idx = 0 in
  let rec aux 

let eval cashed =
  match cached.cache with
  | Some cache -> seqify cache
  | None -> 
