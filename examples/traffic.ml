(* 2D Vector type and utilities *)
type vec2 = {x: float; y: float}

let vec2_add v1 v2 = {x= v1.x +. v2.x; y= v1.y +. v2.y}

let vec2_sub v1 v2 = {x= v1.x -. v2.x; y= v1.y -. v2.y}

let vec2_scale v s = {x= v.x *. s; y= v.y *. s}

let vec2_dot v1 v2 = (v1.x *. v2.x) +. (v1.y *. v2.y)

let vec2_magnitude v = sqrt ((v.x *. v.x) +. (v.y *. v.y))

let vec2_magnitude_sq v = (v.x *. v.x) +. (v.y *. v.y)

let vec2_normalize v =
  let mag = vec2_magnitude v in
  if mag > 0.0001 then {x= v.x /. mag; y= v.y /. mag} else {x= 0.0; y= 0.0}

let vec2_limit v max_mag =
  let mag = vec2_magnitude v in
  if mag > max_mag then
    (* Avoid recomputing magnitude in normalize *)
    let scale = max_mag /. mag in
    vec2_scale v scale
  else v

let vec2_zero = {x= 0.0; y= 0.0}

let vec2_distance v1 v2 = vec2_magnitude (vec2_sub v2 v1)

(* Wrap position to stay within world bounds *)
let vec2_wrap v world_size =
  let wrap_coord c size =
    let c' = mod_float c size in
    if c' < 0.0 then c' +. size else c'
  in
  {x= wrap_coord v.x world_size; y= wrap_coord v.y world_size}

(* Calculate wrapped distance (considering toroidal topology) *)
let vec2_wrapped_delta v1 v2 world_size =
  let wrap_delta d size =
    let half = size /. 2.0 in
    if d > half then d -. size else if d < -.half then d +. size else d
  in
  let dx = wrap_delta (v2.x -. v1.x) world_size in
  let dy = wrap_delta (v2.y -. v1.y) world_size in
  {x= dx; y= dy}

(* Rotate a vector by an angle in radians *)
let vec2_rotate v angle =
  let cos_a = cos angle in
  let sin_a = sin angle in
  {x= (v.x *. cos_a) -. (v.y *. sin_a); y= (v.x *. sin_a) +. (v.y *. cos_a)}

let rvf a b =
  let low = min a b in
  let high = max a b in
  Random.float (high -. low) +. low

(* 2D Bicycle with position, velocity, and acceleration vectors *)
type bicycle =
  { id: int
  ; position: vec2
  ; velocity: vec2
  ; acceleration: vec2
  ; stability_counter: int (* Counts frames of stability *) }

let rec_size = 44100 * 240

let number_of_chans = 16

let of_vec vec2 = (vec2.x, vec2.y)

(* 2D grid using integer pairs as keys *)
module GridKey = struct
  type t = int * int

  let compare = compare
end

module GridMap = Map.Make (GridKey)

type grid = bicycle list GridMap.t

(* Convert grid back to a flat list of bicycles *)
let grid_to_list grid =
  GridMap.fold
    (fun _cell_idx bicycles acc -> List.rev_append bicycles acc)
    grid []

type grid_params = {cell_size: float; world_size: float}

let cell_index params position =
  let max_cells = int_of_float (params.world_size /. params.cell_size) in
  let wrap_cell c max_c =
    let c' = c mod max_c in
    if c' < 0 then c' + max_c else c'
  in
  let x = int_of_float (position.x /. params.cell_size) in
  let y = int_of_float (position.y /. params.cell_size) in
  (wrap_cell x max_cells, wrap_cell y max_cells)

(* Build the grid from a list of bicycles *)
let build_grid params bicycles =
  List.fold_left
    (fun grid bicycle ->
      let idx = cell_index params bicycle.position in
      let cell_bicycles =
        match GridMap.find_opt idx grid with
        | None -> [bicycle]
        | Some bicycles -> bicycle :: bicycles
      in
      GridMap.add idx cell_bicycles grid )
    GridMap.empty bicycles

(* Get bicycles in neighboring cells (9 cells in 2D) with wrapping *)
let get_nearby_bicycles grid params bicycle =
  let cx, cy = cell_index params bicycle.position in
  let max_cells = int_of_float (params.world_size /. params.cell_size) in
  (* Wrap cell indices to handle toroidal topology *)
  let wrap_cell c max_c =
    let c' = c mod max_c in
    if c' < 0 then c' + max_c else c'
  in
  let cells_to_check =
    [ (cx - 1, cy - 1)
    ; (cx, cy - 1)
    ; (cx + 1, cy - 1)
    ; (cx - 1, cy)
    ; (cx, cy)
    ; (cx + 1, cy)
    ; (cx - 1, cy + 1)
    ; (cx, cy + 1)
    ; (cx + 1, cy + 1) ]
  in
  (* Wrap each cell coordinate before lookup *)
  let wrapped_cells =
    List.map
      (fun (x, y) -> (wrap_cell x max_cells, wrap_cell y max_cells))
      cells_to_check
  in
  List.fold_left
    (fun acc cell_idx ->
      match GridMap.find_opt cell_idx grid with
      | None -> acc
      | Some bicycles -> bicycles @ acc )
    [] wrapped_cells

type model =
  { grid: grid
  ; grid_params: grid_params
  ; desired_speed: float
  ; max_speed: float
  ; max_force: float
  ; separation_radius: float
  ; alignment_radius: float
  ; cohesion_radius: float
  ; rotation: float
  ; dt: float
  ; world_size: float
  ; next_id: int
  ; recording: float Array.t Array.t }

let mkBicycle world_size id =
  let angle = Random.float (2.0 *. Float.pi) in
  let speed = 30.0 in
  { id
  ; position= {x= Random.float world_size; y= Random.float world_size}
  ; velocity= {x= cos angle *. speed; y= sin angle *. speed}
  ; acceleration= vec2_zero
  ; stability_counter= 0 }

type bicycles_with_id = {current_id: int; bicycles: bicycle list}

let mkBicycles world_size count =
  {current_id= count; bicycles= List.init count (mkBicycle world_size)}

let zero_id = 0

let init () =
  let world_size = 400.0 in
  let pars = {cell_size= 30.0; world_size} in
  let initial_bicycles = mkBicycles world_size 49 in
  { grid= build_grid pars initial_bicycles.bicycles
  ; grid_params= pars
  ; desired_speed= 80.0
  ; max_speed= 60.0
  ; max_force= 150.0
  ; separation_radius= 5.0
  ; alignment_radius= 30.0
  ; cohesion_radius= 5.0
  ; rotation= 0.0
  ; dt= 0.01
  ; world_size
  ; next_id= initial_bicycles.current_id
  ; recording= Array.init number_of_chans (fun _ -> Array.make rec_size 0.0) }

(* Steering behaviors for collision avoidance *)

(* Separation: steer to avoid crowding local bicycles *)
let separation model bicycle nearby =
  let sep_radius_sq = model.separation_radius *. model.separation_radius in
  let steering =
    List.fold_left
      (fun acc other ->
        if other.id = bicycle.id then acc
        else
          (* Delta points FROM other TO bicycle (away direction) *)
          let delta =
            vec2_wrapped_delta other.position bicycle.position model.world_size
          in
          let dist_sq = vec2_magnitude_sq delta in
          if dist_sq > 0.0 && dist_sq < sep_radius_sq then
            (* Steer away from nearby bicycle, stronger when closer *)
            let dist = sqrt dist_sq in
            let diff = vec2_scale delta (1.0 /. dist) in
            (* normalize without recomputing *)
            (* Already pointing away, just normalize *)
            let weight =
              (model.separation_radius -. dist) /. model.separation_radius
            in
            vec2_add acc (vec2_scale diff weight)
          else acc )
      vec2_zero nearby
  in
  let mag = vec2_magnitude steering in
  if mag > 0.0 then
    (* Scale to max_force *)
    let scale = model.max_force *. 1.5 /. mag in
    vec2_scale steering scale (* Separation is important *)
  else vec2_zero

(* Alignment: steer towards average heading of local bicycles *)
let alignment model bicycle nearby =
  let align_radius_sq = model.alignment_radius *. model.alignment_radius in
  let sum, count =
    List.fold_left
      (fun (sum, count) other ->
        if other.id = bicycle.id then (sum, count)
        else
          let delta =
            vec2_wrapped_delta bicycle.position other.position model.world_size
          in
          let dist_sq = vec2_magnitude_sq delta in
          if dist_sq < align_radius_sq then
            (vec2_add sum other.velocity, count + 1)
          else (sum, count) )
      (vec2_zero, 0) nearby
  in
  if count > 0 then
    let avg = vec2_scale sum (1.0 /. float_of_int count) in
    let avg_mag = vec2_magnitude avg in
    let desired =
      if avg_mag > 0.0001 then vec2_scale avg (model.desired_speed /. avg_mag)
      else vec2_zero
    in
    let steer = vec2_sub desired bicycle.velocity in
    vec2_limit steer model.max_force
  else vec2_zero

(* Cohesion: steer towards average position of local bicycles *)
let cohesion model bicycle nearby =
  let coh_radius_sq = model.cohesion_radius *. model.cohesion_radius in
  let sum, count =
    List.fold_left
      (fun (sum, count) other ->
        if other.id = bicycle.id then (sum, count)
        else
          let delta =
            vec2_wrapped_delta bicycle.position other.position model.world_size
          in
          let dist_sq = vec2_magnitude_sq delta in
          if dist_sq < coh_radius_sq then
            (vec2_add sum other.position, count + 1)
          else (sum, count) )
      (vec2_zero, 0) nearby
  in
  if count > 0 then
    let avg = vec2_scale sum (1.0 /. float_of_int count) in
    let delta = vec2_wrapped_delta bicycle.position avg model.world_size in
    let delta_mag = vec2_magnitude delta in
    let desired =
      if delta_mag > 0.0001 then
        vec2_scale delta (model.desired_speed /. delta_mag)
      else vec2_zero
    in
    let steer = vec2_sub desired bicycle.velocity in
    vec2_limit steer model.max_force
  else vec2_zero

(* Update a single bicycle *)
let update_bicycle model bicycle nearby clock =
  (* Calculate steering forces *)
  let sep = separation model bicycle nearby in
  let align = alignment model bicycle nearby in
  let coh = cohesion model bicycle nearby in
  (* Combine forces with weights *)
  let total_force =
    vec2_zero
    |> vec2_add (vec2_scale sep 2.0) (* Separation is most important *)
    |> vec2_add (vec2_scale align 1.0)
    |> vec2_add (vec2_scale coh 1.0)
  in
  (* Apply force to get acceleration *)
  let new_acceleration = total_force in
  (* Update velocity *)
  let new_velocity =
    vec2_add bicycle.velocity (vec2_scale new_acceleration model.dt)
  in
  let new_velocity = vec2_limit new_velocity model.max_speed in
  (* Stability detection: measure if acceleration/steering has been minimal *)
  let acceleration_magnitude = vec2_magnitude new_acceleration in
  let stability_threshold = 50.0 in
  (* Threshold for "stable" state *)
  let stability_trigger = 100 in
  (* Frames of stability before turning *)
  let new_stability_counter =
    if acceleration_magnitude < stability_threshold then
      bicycle.stability_counter + 1
    else 0 (* Reset counter if significant steering detected *)
  in
  (* Apply anti-stability turn when stable for too long *)
  let new_velocity_with_turn clock =
    if new_stability_counter >= stability_trigger then
      (* Turn right by 45 degrees (π/4 radians) *)
      let turn_angle = 2.0 *. Float.pi *. model.rotation in
      vec2_rotate new_velocity turn_angle
    else new_velocity
  in
  (* Reset counter after applying turn *)
  let final_stability_counter =
    if new_stability_counter >= stability_trigger then 0
    else new_stability_counter
  in
  let new_vol = new_velocity_with_turn clock in
  (* Update position *)
  let new_position = vec2_add bicycle.position (vec2_scale new_vol model.dt) in
  let new_position = vec2_wrap new_position model.world_size in
  { bicycle with
    position= new_position
  ; velocity= new_vol
  ; acceleration= new_acceleration
  ; stability_counter= final_stability_counter }

let update_ants_with_osc id x_pos y_pos =
  (* Create a UDP client *)
  let client = Osc_unix.Udp.Client.create () in
  let open Osc.OscTypes in
  (* Build the OSC message *)
  let msg =
    { address= "/point"
    ; arguments=
        [ Int32 (Int32.of_int id)
        ; (* i *)
          Float32 x_pos
        ; (* f *)
          Float32 y_pos (* f *) ] }
  in
  let packet = Message msg in
  let addr = Unix.inet_addr_of_string "127.0.0.1" in
  let port = 57666 in
  let inet = Unix.ADDR_INET (addr, port) in
  (* Send it *)
  Osc_unix.Udp.Client.send client inet packet

let update_all_ants_with_osc all_bicycles =
  (* Create a UDP client *)
  let scale_factor = 1.0 /. 400.0 in
  let client = Osc_unix.Udp.Client.create () in
  let open Osc.OscTypes in
  let data_args =
    List.concat_map
      (fun bicycle -> [Float32 (bicycle.position.x *. scale_factor); Float32 (bicycle.position.y *. scale_factor)])
      all_bicycles
  in
  (* let _ = Printf.printf "Encoded %d bicycles in OSC message" (List.length data_args) in *)
  (* Build the OSC message *)
  let msg = {address= "/all_points"; arguments= data_args} in
  let packet = Message msg in
  let addr = Unix.inet_addr_of_string "127.0.0.1" in
  let port = 57666 in
  let inet = Unix.ADDR_INET (addr, port) in
  (* Send it *)
  Osc_unix.Udp.Client.send client inet packet

(* Web-based Visualization *)
let html_file = "/tmp/traffic_viz.html"

let data_file = "/tmp/traffic_data.json"

let params_file = "/tmp/traffic_params.json"

(* Read parameters from JSON file if it exists *)
let read_params model =
  try
    let json = Yojson.Basic.from_file params_file in
    let open Yojson.Basic.Util in
    let new_model =
      { model with
        max_speed= json |> member "max_speed" |> to_number
      ; max_force= json |> member "max_force" |> to_number
      ; separation_radius= json |> member "separation_radius" |> to_number
      ; alignment_radius= json |> member "alignment_radius" |> to_number
      ; cohesion_radius= json |> member "cohesion_radius" |> to_number
      ; rotation= json |> member "rotation_amount" |> to_number }
    in
    (* Debug output *)
    (* Printf.printf "Params updated: speed=%.1f force=%.1f sep=%.1f align=%.1f coh=%.1f rot=%.1f \n%!"
      new_model.max_speed new_model.max_force new_model.separation_radius
      new_model.alignment_radius new_model.cohesion_radius new_model.rotation ; *)
    new_model
  with
  | e ->
      Printf.printf "Failed to read params: %s\n%!" (Printexc.to_string e) ;
      model

let create_html_file () =
  (* Read HTML template from file and copy it to /tmp *)
  let template_file = "examples/traffic_viz.html" in
  let ic = open_in template_file in
  let html = really_input_string ic (in_channel_length ic) in
  close_in ic ;
  let oc = open_out html_file in
  output_string oc html ; close_out oc

let visualize_web model step =
  let bicycles = grid_to_list model.grid in
  let avg_speed =
    List.fold_left (fun sum b -> sum +. vec2_magnitude b.velocity) 0.0 bicycles
    /. float_of_int (List.length bicycles)
  in
  (* Create JSON data *)
  let json_bicycles =
    String.concat ",\n    "
      (List.map
         (fun b ->
           Printf.sprintf
             {|{"id": %d, "position": {"x": %.2f, "y": %.2f}, "velocity": {"x": %.2f, "y": %.2f}, "stability_counter": %d}|}
             b.id b.position.x b.position.y b.velocity.x b.velocity.y
             b.stability_counter )
         bicycles )
  in
  let json =
    Printf.sprintf
      {|{
  "step": %d,
  "count": %d,
  "avg_speed": %.2f,
  "world_size": %.2f,
  "max_speed": %.2f,
  "bicycles": [
    %s
  ]
}|}
      step (List.length bicycles) avg_speed model.world_size model.max_speed
      json_bicycles
  in
  (* Write JSON to file *)
  let oc = open_out data_file in
  output_string oc json ; close_out oc

let setup_web_viz () =
  create_html_file () ;
  (* Kill any existing servers on port 8765 *)
  let _ = Sys.command "pkill -f 'python3.*8765' 2>/dev/null || true" in
  Unix.sleepf 0.5 ;
  (* Start Flask server in the background *)
  let port = 8765 in
  let server_cmd =
    Printf.sprintf
      "python3 /tmp/traffic_server.py > /tmp/flask_server.log 2>&1 &"
  in
  let _ = Printf.printf "Starting flask server with:  %s" server_cmd in
  let r = Sys.command server_cmd in
  let _ = Printf.printf "server cmd completed with: %d" r; flush stdout in
  Unix.sleepf 1.5 ;
  (* Give server time to start *)
  (* Open in browser *)
  let _ = Sys.command (Printf.sprintf "open http://localhost:%d/" port) in
  ()

(* Simulation step *)
let step model clock =
  let bicycles = grid_to_list model.grid in
  (* let _ = Unix.sleepf 0.1 in *)
  let updated_bicycles =
    List.map
      (fun bicycle ->
        let nearby = get_nearby_bicycles model.grid model.grid_params bicycle in
        update_bicycle model bicycle nearby clock )
      bicycles
  in
  let new_grid = build_grid model.grid_params updated_bicycles in
  {model with grid= new_grid}

(* Add a new bicycle to the model *)
let add_bicycle model =
  let new_bicycle = mkBicycle model.world_size model.next_id in
  let bicycles = grid_to_list model.grid @ [new_bicycle] in
  let new_grid = build_grid model.grid_params bicycles in
  {model with grid= new_grid; next_id= model.next_id + 1}

let clear_bicycles model =
  let new_bicycle = mkBicycle model.world_size model.next_id in
  let new_grid = build_grid model.grid_params [new_bicycle] in
  {model with grid= new_grid; next_id= model.next_id + 1}

(* Terminal handling *)
let saved_term_attrs = ref None

let setup_terminal () =
  try
    let term_attrs = Unix.tcgetattr Unix.stdin in
    saved_term_attrs := Some term_attrs ;
    let raw_attrs =
      { term_attrs with
        Unix.c_icanon= false
      ; Unix.c_echo= false
      ; Unix.c_vmin= 0
      ; Unix.c_vtime= 0 }
    in
    Unix.tcsetattr Unix.stdin Unix.TCSANOW raw_attrs
  with
  | Unix.Unix_error _ -> () (* Terminal setup not supported, skip it *)

let restore_terminal () =
  match !saved_term_attrs with
  | Some attrs -> Unix.tcsetattr Unix.stdin Unix.TCSANOW attrs
  | None -> ()

(* Check for keyboard input without blocking *)
let check_input () =
  let ready, _, _ = Unix.select [Unix.stdin] [] [] 0.0 in
  if ready = [] then None
  else
    let buf = Bytes.create 1 in
    let n = Unix.read Unix.stdin buf 0 1 in
    if n = 0 then None else Some (Bytes.get buf 0)

let head lst =
  match lst with
  | [] -> None
  | hd :: ts -> Some hd

let write_audio size arrarr =
  print_string "finished recording" ;
  flush stdout ;
  let slice = Array.map (fun arr -> Array.sub arr 0 size) arrarr in
  Sndfile.write_multichannel_array slice 44100 "boid2.wav" Sndfile.WAV_24

let vec2sound vec2 =
  vec2.position |> of_vec |> Cisp.fst |> Cisp.linlin 0.0 400.0 (-1.0) 1.0

let record_frame idx model =
  if idx >= rec_size - 1 then write_audio idx model.recording else () ;
  let bicycles = model.grid |> grid_to_list in
  (* Build lookup array indexed by bicycle ID for O(1) access *)
  let max_id = model.next_id in
  let bicycle_lookup = Array.make max_id None in
  List.iter (fun b -> bicycle_lookup.(b.id) <- Some b) bicycles ;
  (* Record each channel *)
  for channel = 0 to number_of_chans - 1 do
    let bicycle_id = channel + 0 in
    let value =
      if bicycle_id < max_id then
        match bicycle_lookup.(bicycle_id) with
        | None -> 0.0
        | Some bicycle -> vec2sound bicycle
      else 0.0
    in
    model.recording.(channel).(idx) <- value
  done

(* Main simulation loop with visualization *)
let rec simulate model n current_step =
  Unix.sleepf 0.1 ;
  if n = 0 then model
  else (
    if n mod 1 = 0 then visualize_web model current_step else () ;
    if n mod 1 = 0 then update_all_ants_with_osc (model.grid |> grid_to_list) ;
    (* if n mod 1 = 0 then (
      print_int n ;
       print_endline "ok we are here" ) 
    ) *)
    (* Read parameters from web interface every 100 frames *)
    let model_with_params =
      read_params model
      (* if current_step mod 100 = 0 then read_params model else model *)
    in
    (* Check for keyboard input *)
    let model_after_input =
      match check_input () with
      | Some ' ' -> add_bicycle model_with_params
      | Some 'q' ->
          Printf.printf "Quitting...\n" ;
          restore_terminal () ;
          exit 0
      | Some 'c' ->
          Printf.printf "Clear all\n" ;
          clear_bicycles model_with_params
      | Some 'r' ->
          Printf.printf "ok writing" ;
          write_audio (current_step - 1) model_with_params.recording ;
          model_with_params
      | Some 's' ->
          Printf.printf "send points to ants" ;
          update_all_ants_with_osc (model_with_params.grid |> grid_to_list) ;
          model_with_params
      | _ -> model_with_params
    in
    let new_model = step model_after_input current_step in
    let new_model2 = record_frame current_step new_model in
    simulate new_model (n - 1) (current_step + 1) )

(* Run the simulation *)
let () =
  let model = init () in
  Printf.printf "Starting 2D traffic simulation with %d bicycles\n"
    (List.length (grid_to_list model.grid)) ;
  Printf.printf "Press SPACE to add bicycles, 'q' to quit, 'c' to clear\n" ;
  Printf.printf "Opening web visualization in browser...\n" ;
  flush stdout ;
  setup_web_viz () ;
  setup_terminal () ;
  at_exit restore_terminal ;
  let _final_model = simulate model 4410000 0 in
  restore_terminal () ; ()
