
val safe_get : 'a array -> int -> 'a option

(* mini maxi input, clips *)
val clipf : 'a -> 'a -> 'a -> 'a


val map_vertical :
  ('a -> 'a array) -> int -> 'a array array -> 'a array array
(** In a two dimensional array, map vertically
   input = [|
   [|1;2;3|];
   [|4;5;6|];
   [|7;8;9|]
   |]
   map_vertical (fun x -> x * 2) 1 input
   [|
   [|1;4;3|];
   [|4;10;6|];
   [|7;16;9|]
   |]
*)

val lst_take : int -> 'a list -> 'a list

val update_at_index :
  'a array array ->
  int ->
  ('a -> 'a) ->
  'a array array


val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c

val rvfi : float -> float -> float
(** returns a random float *)

val rvi : int -> int -> int
(** returns a random integer *)
val wrap : int -> int -> int -> int
(** wrap an int between two boundaries *)

val wrapf : float -> float -> float -> float
(** wraps a float between two boundaries
    @low 
    @high 
    @input
 *)
val shuffle : 'a List.t -> 'a List.t
(** *)

val choice : 'a -> 'a List.t -> 'a
(** returns a random pick from a (non empty) list *)

val choice_arr_opt : 'a array -> 'a option
(** choice array, if empty return none *)

val modBy : int -> int -> int
val modByf : float -> float -> float 
val update_nth : 'a list -> int -> 'a -> 'a list



(* tuple *)
val fst : 'a * 'b -> 'a
val sec : 'a * 'b -> 'b 

(* flatten array of arrays *)
val flatten : 'a array array -> 'a array

val write_string_to_file : string -> string -> unit
val generate_timestamp_filename :
  ?prefix:string -> ?suffix:string -> unit -> string

val quote : string -> string





 