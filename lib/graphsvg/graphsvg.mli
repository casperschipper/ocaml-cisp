type point = float * float
type edge = { p1 : int; p2 : int; strength : float }

val flatten : 'a array array -> 'a array
val edges_from_arrayarray : float array array -> edge array

val normalize :
  float -> float -> float -> float -> float -> float

val svg_line :
  float * float ->
  float * float ->
  edge ->
  float ->
  float ->
  string

val svg_circle : float * float -> string

val generate_svg :
  (float * float) array -> float array array -> float -> string
