type 'a t = {front: 'a list; back: 'a list}

let empty = {front= []; back= []}

let is_empty = function {front= []; back= []} -> true | _ -> false

(* Helper function to ensure that a queue is in normal form. *)
let norm = function
  | {front= []; back} -> {front= List.rev back; back= []}
  | q -> q

let enqueue x q = norm {q with back= x :: q.back}

let peek = function {front= []; _} -> None | {front= x :: _; _} -> Some x

let dequeue = function
  | {front= []; _} -> empty
  | {front= _ :: xs; back} -> norm {front= xs; back}

let dequeue = function
    | {front=[]; _} -> None
    | {front=_::xs; back} -> Some (norm {front=xs; back})

let rec to_list q =
  let front = peek q in
  match front with None -> [] | Some v -> v :: to_list (dequeue q)

let peekdeq q =
  match peek q with
  | Some x -> Some (x, dequeue q)
  | None -> None 
