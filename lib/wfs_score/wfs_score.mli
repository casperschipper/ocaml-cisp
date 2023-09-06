type env = Normal of { fade_in : float; fade_out : float; }
type sound = Simple_sine of float
type position = { x : float; y : float; }
type event = {
  start : float;
  duration : float;
  envelope : env;
  sound : sound;
  position : position;
}
type score = event list
val position_to_point_str : position -> string
val to_string : event -> string
