type t = Node of {age: int; value: int}

let value (Node {value; _}) = value

let compare a b = Int.compare (value a) (value b)

let isOld (Node _) = false

let age (Node n) = Node {n with age= n.age + 1}

let node (value : int) = Node {age= 0; value}

let print (Node {age; value}) =
  Printf.printf "Node { age= %i; value= %i }\n" age value ;
  print_newline ()
