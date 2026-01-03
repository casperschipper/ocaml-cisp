(** Example: Using the Params module to generate and send traffic parameters

    This example demonstrates how to use applicative-style parameter
    generation to control the traffic simulation dynamically.
*)

open Params

(** Example 1: Simple oscillating parameters *)
let example_oscillating () =
  let param_builder =
    lift5 (fun speed force sep align coh ->
      `Assoc [
        ("max_speed", `Float speed);
        ("max_force", `Float force);
        ("separation_radius", `Float sep);
        ("alignment_radius", `Float align);
        ("cohesion_radius", `Float coh);
        ("rotation_amount", `Float 0.125);
      ])
      (oscillate ~min:20.0 ~max:100.0 ~period:50.0)
      (const 150.0)
      (oscillate ~min:3.0 ~max:15.0 ~period:30.0)
      (const 30.0)
      (const 5.0)
  in
  Lwt_main.run (run_loop default_config param_builder Yojson.Basic.to_string)

(** Example 2: Random walk parameters *)
let example_random_walk () =
  let param_builder =
    lift5 (fun speed force sep align coh ->
      `Assoc [
        ("max_speed", `Float speed);
        ("max_force", `Float force);
        ("separation_radius", `Float sep);
        ("alignment_radius", `Float align);
        ("cohesion_radius", `Float coh);
        ("rotation_amount", `Float 0.125);
      ])
      (random_walk ~start:60.0 ~step_size:5.0 |> map (Float.max 10.0) |> map (Float.min 150.0))
      (const 150.0)
      (random_walk ~start:5.0 ~step_size:1.0 |> map (Float.max 1.0) |> map (Float.min 20.0))
      (random_walk ~start:30.0 ~step_size:3.0 |> map (Float.max 5.0) |> map (Float.min 60.0))
      (const 5.0)
  in
  Lwt_main.run (run_loop default_config param_builder Yojson.Basic.to_string)

(** Example 3: Cycling through presets *)
let example_presets () =
  (* Define some interesting presets *)
  let presets = [
    ("loose", 80.0, 200.0, 3.0, 40.0, 3.0);
    ("tight", 40.0, 100.0, 10.0, 20.0, 8.0);
    ("fast", 120.0, 250.0, 5.0, 30.0, 5.0);
    ("slow", 30.0, 80.0, 8.0, 25.0, 6.0);
  ] in

  let param_builder =
    let preset_seq = List.to_seq presets |> Seq.cycle in
    of_seq preset_seq |> map (fun (name, speed, force, sep, align, coh) ->
      Printf.printf "Switching to preset: %s\n%!" name;
      `Assoc [
        ("max_speed", `Float speed);
        ("max_force", `Float force);
        ("separation_radius", `Float sep);
        ("alignment_radius", `Float align);
        ("cohesion_radius", `Float coh);
        ("rotation_amount", `Float 0.125);
      ]
    )
  in
  let config = { url = "http://localhost:8765/update_params"; interval = 5.0 } in
  Lwt_main.run (run_loop config param_builder Yojson.Basic.to_string)

(** Example 4: Complex choreography *)
let example_choreography () =
  (* Create a choreographed sequence of behaviors *)
  let param_builder =
    lift5 (fun speed force sep align rot ->
      `Assoc [
        ("max_speed", `Float speed);
        ("max_force", `Float force);
        ("separation_radius", `Float sep);
        ("alignment_radius", `Float align);
        ("cohesion_radius", `Float 5.0);
        ("rotation_amount", `Float rot);
      ])
      (* Speed oscillates slowly *)
      (oscillate ~min:30.0 ~max:100.0 ~period:100.0)
      (* Force has fast variations *)
      (oscillate ~min:100.0 ~max:250.0 ~period:20.0)
      (* Separation radius walks randomly *)
      (random_walk ~start:5.0 ~step_size:0.5 |> map (Float.max 2.0) |> map (Float.min 15.0))
      (* Alignment radius cycles through values *)
      (cycle [20.0; 30.0; 40.0; 50.0])
      (* Rotation amount oscillates *)
      (oscillate ~min:0.0 ~max:0.25 ~period:60.0)
  in
  let config = { url = "http://localhost:8765/update_params"; interval = 0.5 } in
  Lwt_main.run (run_loop config param_builder Yojson.Basic.to_string)

let () =
  (* Choose which example to run based on command line argument *)
  let mode = if Array.length Sys.argv > 1 then Sys.argv.(1) else "oscillating" in
  match mode with
  | "oscillating" ->
      Printf.printf "Running oscillating parameters example\n";
      example_oscillating ()
  | "random" ->
      Printf.printf "Running random walk parameters example\n";
      example_random_walk ()
  | "presets" ->
      Printf.printf "Running preset cycling example\n";
      example_presets ()
  | "choreography" ->
      Printf.printf "Running complex choreography example\n";
      example_choreography ()
  | _ ->
      Printf.printf "Unknown mode. Available: oscillating, random, presets, choreography\n";
      exit 1
