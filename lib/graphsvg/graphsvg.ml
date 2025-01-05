(* Define a point as a pair of floats *)
type point = float * float

(* Edge structure holding indices of points and a strength value *)
type edge = {p1: int; p2: int; strength: float}

let flatten arr = arr |> Array.fold_left Array.append [||]

let edges_from_arrayarray arrr =
  arrr
  |> Array.mapi (fun i arr ->
         arr |> Array.mapi (fun j strength -> {p1= i; p2= j; strength}) )
  |> flatten

(* Normalize a value between a given min and max *)
let normalize value min_val max_val min_range max_range =
  let ratio = (value -. min_val) /. (max_val -. min_val) in
  min_range +. (ratio *. (max_range -. min_range))

(* let replaceNan flt =
   if flt = nan then
     0.0
   else
     flt *)

(* Function to generate SVG string for a line with normalized thickness and opacity *)
let svg_line p1 p2 edge min_strength max_strength =
  let x1, y1 = p1 in
  let x2, y2 = p2 in
  let thickness = normalize edge.strength min_strength max_strength 0.1 2.0 in
  let opacity = normalize edge.strength min_strength max_strength 0.1 1.0 in
  Printf.sprintf
    "<line x1=\"%.2f\" y1=\"%.2f\" x2=\"%.2f\" y2=\"%.2f\" stroke=\"black\" \
     stroke-width=\"%.2f\" stroke-opacity=\"%.2f\" />\n"
    x1 y1 x2 y2 thickness opacity

(* Function to generate SVG string for a circle at a given point *)
let svg_circle (x, y) =
  Printf.sprintf "<circle cx=\"%.2f\" cy=\"%.2f\" r=\"3\" fill=\"blue\" />\n" x
    y

(* Main function to generate SVG from points and edges *)
let generate_svg points pheromones shortest =
  let edges = pheromones |> edges_from_arrayarray in
  (* Get the min and max edge strength to normalize thickness and opacity *)
  let strengths = Array.map (fun e -> e.strength) edges in
  let min_strength = Array.fold_left min (Array.get strengths 0) strengths in
  let max_strength = Array.fold_left max (Array.get strengths 0) strengths in
  (* Start building the SVG string *)
  let svg_header =
    "<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" width=\"1000\" \
     height=\"800\" viewBox=\"0 0 150 150\">\n"
  in
  let svg_footer = "</svg>\n" in
  (* Add text for min and max strength values *)
  let svg_text =
    Printf.sprintf
      "<text x=\"10\" y=\"100\" font-size=\"5\" fill=\"black\">Min Strength: \
       %.2f</text>\n\
       <text x=\"10\" y=\"105\" font-size=\"5\" fill=\"black\">Max Strength: \
       %.2f</text>\n\
       <text x=\"10\" y=\"110\" font-size=\"5\" fill=\"black\">Shortest: \
       %.2f</text>\n"
      min_strength max_strength shortest
  in
  let scaled_points = points |> Array.map (fun (x,y) -> (x *. 100.0,y *. 100.0)) in
  (* Create SVG elements for points and edges *)
  let svg_circles =
    Array.map svg_circle scaled_points |> Array.to_list |> String.concat ""
  in
  let svg_lines =
    edges
    |> Array.map (fun edge ->
           let p1 = scaled_points.(edge.p1) in
           (* pretty safe because map *)
           let p2 = scaled_points.(edge.p2) in
           svg_line p1 p2 edge min_strength max_strength )
    |> Array.to_list |> String.concat ""
  in
  (* Combine everything into the final SVG string *)
  [svg_header; svg_circles; svg_lines; svg_text; svg_footer] |> String.concat ""

(* Example usage *)
(* let points = [|(10.0, 10.0); (100.0, 100.0); (150.0, 50.0); (200.0, 200.0)|] *)
(* Example points *)

(* let edges =
   Array.init 4 (fun i -> Array.init 4 (fun j -> { p1 = i ; p2 = j; strength =  Random.float 10.0})) *)
(* Example edges *)

(* let () =
   let svg_content = generate_svg points edges in
   Printf.printf "%s" svg_content *)
