module type ISeq_type = sig
  type +'a inf_node =
        InfCons of 'a * 'a t
  and 'a t =
    unit -> 'a inf_node

  val countFrom : int -> int t
  val cycleSq : 'a Seq.t -> 'a t
  val take : int -> 'a t -> 'a Seq.t
  val concatSq : 'a Seq.t t -> 'a t
end

module ISeq : ISeq_type = struct
  type +'a inf_node =
        InfCons of 'a * 'a t
  and 'a t =
    unit -> 'a inf_node
  
  let rec countFrom n () =
    InfCons(n, countFrom (n + 1))

  let rec repeat x () =
    InfCons(x, repeat x)

  let rec take n isq () =
    if n <= 0 then
      Seq.Nil
    else
      let (InfCons (h, tl)) = isq () in
      Seq.Cons(h,take (n-1) tl)

  let rec concatSq ssq () =
    match ssq () with 
      InfCons (h, ls) -> (
      match h () with
      | Seq.Cons (h', ls') ->
         let newtail () = InfCons (ls', ls) in
         InfCons (h', concatSq newtail)
      | Seq.Nil -> concatSq ls () )
  
  let cycleSq sq =
    repeat sq |> concatSq

 end
  
     
