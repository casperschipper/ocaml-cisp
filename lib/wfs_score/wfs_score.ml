type env = Normal of { fade_in : float; fade_out : float }
type sound = Simple_sine of float
type position = { x : float; y : float }
type event = { start : float; duration : float; envelope : env; sound : sound ; position : position }
type score = event list

let position_to_point_str { x ; y } =
  "Point("^Float.to_string x^","^Float.to_string y^")"

let to_string { start; duration; envelope; sound; position } =
  match sound with
  | Simple_sine freq ->
    let fstr = freq |> Float.to_string in
    let starts = Float.to_string start in
    let durstr = Float.to_string duration in
    let _ = envelope in
      "UChain("^starts^", 0.0, "^durstr^", [ 'sine', [ 'freq', "^fstr^", 'phase', \
       -0.25132741228718, 'amp', 0.2 ] ], [ 'wfsSource', [ 'point', "^position_to_point_str position^" ] ]))"
