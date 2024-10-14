let generate_svg (arr : float array array) : string =
  let num_rows = Array.length arr in
  let num_cols = Array.length arr.(0) in

  (* Find the maximum value in the array *)
  let max_value =
    Array.fold_left
      (fun acc row ->
         Array.fold_left (fun max_val v -> max max_val v) acc row)
      min_float arr
  in

  (* Define SVG dimensions and cell size *)
  let svg_size = 800 in
  let cell_size = float_of_int svg_size /. float_of_int (max num_rows num_cols) in

  (* Function to normalize value to a shade of gray *)
  let normalize_value v =
    if max_value = 0.0 then 0.0
    else v /. max_value
  in

  let value_to_color v =
    let norm = normalize_value v in
    let gray = int_of_float (255.0 *. (1.0 -. norm)) in
    Printf.sprintf "#%02x%02x%02x" gray gray gray
  in

  (* Generate SVG content *)
  let svg_header = Printf.sprintf
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n\
    <svg width=\"%d\" height=\"%d\" xmlns=\"http://www.w3.org/2000/svg\">\n"
    svg_size svg_size
  in
  let svg_footer = "</svg>\n" in

  let cells =
    Array.mapi (fun i row ->
      Array.mapi (fun j value ->
        let x = float_of_int j *. cell_size in
        let y = float_of_int i *. cell_size in
        let color = value_to_color value in
        Printf.sprintf
          "  <rect x=\"%f\" y=\"%f\" width=\"%f\" height=\"%f\" fill=\"%s\"/>\n"
          x y cell_size cell_size color
      ) row
    ) arr
    |> Array.to_list          (* Convert the outer array to a list of arrays *)
  |> List.map Array.to_list (* Convert each inner array to a list *)
  |> List.concat            (* Concatenate the lists into a single list of strings *)
  |> String.concat ""       (* Combine all strings into a single string *)
  in

  svg_header ^ cells ^ svg_footer

(* Example usage *)
let () =
  let arr = [|
    [| 1.0; 0.5; 0.25 |];
    [| 0.75; 0.0; 0.5 |];
    [| 0.25; 0.5; 1.0 |]
  |] in
  let svg_string = generate_svg arr in
  print_endline svg_string
