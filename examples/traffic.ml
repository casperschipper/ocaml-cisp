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
  if mag > max_mag then vec2_scale (vec2_normalize v) max_mag else v

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
  {x= v.x *. cos_a -. v.y *. sin_a; y= v.x *. sin_a +. v.y *. cos_a}

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
  ; stability_counter: int  (* Counts frames of stability *)
  }

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
  GridMap.fold (fun _cell_idx bicycles acc -> bicycles @ acc) grid []

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

let mkBicycles world_size count = List.init count (mkBicycle world_size)

let init () =
  let world_size = 400.0 in
  let pars = {cell_size= 30.0; world_size} in
  { grid= build_grid pars (mkBicycles world_size 333)
  ; grid_params= pars
  ; desired_speed= 80.0
  ; max_speed= 60.0
  ; max_force= 150.0
  ; separation_radius= 5.0
  ; alignment_radius= 30.0
  ; cohesion_radius= 5.0
  ; dt= 0.01
  ; world_size
  ; next_id= 10
  ; recording= Array.init number_of_chans (fun _ -> Array.make rec_size 0.0)
  }

(* Steering behaviors for collision avoidance *)

(* Separation: steer to avoid crowding local bicycles *)
let separation model bicycle nearby =
  let steering =
    List.fold_left
      (fun acc other ->
        if other.id = bicycle.id then acc
        else
          (* Delta points FROM other TO bicycle (away direction) *)
          let delta =
            vec2_wrapped_delta other.position bicycle.position model.world_size
          in
          let dist = vec2_magnitude delta in
          if dist > 0.0 && dist < model.separation_radius then
            (* Steer away from nearby bicycle, stronger when closer *)
            let diff = vec2_normalize delta in
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
    let normalized = vec2_normalize steering in
    vec2_scale normalized (model.max_force *. 1.5) (* Separation is important *)
  else vec2_zero

(* Alignment: steer towards average heading of local bicycles *)
let alignment model bicycle nearby =
  let sum, count =
    List.fold_left
      (fun (sum, count) other ->
        if other.id = bicycle.id then (sum, count)
        else
          let delta =
            vec2_wrapped_delta bicycle.position other.position model.world_size
          in
          let dist = vec2_magnitude delta in
          if dist < model.alignment_radius then
            (vec2_add sum other.velocity, count + 1)
          else (sum, count) )
      (vec2_zero, 0) nearby
  in
  if count > 0 then
    let avg = vec2_scale sum (1.0 /. float_of_int count) in
    let desired = vec2_scale (vec2_normalize avg) model.desired_speed in
    let steer = vec2_sub desired bicycle.velocity in
    vec2_limit steer model.max_force
  else vec2_zero

(* Cohesion: steer towards average position of local bicycles *)
let cohesion model bicycle nearby =
  let sum, count =
    List.fold_left
      (fun (sum, count) other ->
        if other.id = bicycle.id then (sum, count)
        else
          let delta =
            vec2_wrapped_delta bicycle.position other.position model.world_size
          in
          let dist = vec2_magnitude delta in
          if dist < model.cohesion_radius then
            (vec2_add sum other.position, count + 1)
          else (sum, count) )
      (vec2_zero, 0) nearby
  in
  if count > 0 then
    let avg = vec2_scale sum (1.0 /. float_of_int count) in
    let delta = vec2_wrapped_delta bicycle.position avg model.world_size in
    let desired = vec2_scale (vec2_normalize delta) model.desired_speed in
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
  let stability_threshold = 50.0 in  (* Threshold for "stable" state *)
  let stability_trigger = 100 in     (* Frames of stability before turning *)

  let new_stability_counter =
    if acceleration_magnitude < stability_threshold then
      bicycle.stability_counter + 1
    else
      0  (* Reset counter if significant steering detected *)
  in

  (* Apply anti-stability turn when stable for too long *)
  let new_velocity_with_turn clock =
    if new_stability_counter >= stability_trigger then
      (* Turn right by 45 degrees (π/4 radians) *)
      let turn_angle = Float.pi /. (sin (float_of_int clock /. 4410.0 *. Float.pi *. 2.0)) in
      vec2_rotate new_velocity turn_angle
    else
      new_velocity
  in

  (* Reset counter after applying turn *)
  let final_stability_counter =
    if new_stability_counter >= stability_trigger then 0
    else new_stability_counter
  in

  let new_vol = new_velocity_with_turn clock in

  (* Update position *)
  let new_position =
    vec2_add bicycle.position (vec2_scale new_vol model.dt)
  in
  let new_position = vec2_wrap new_position model.world_size in
  { bicycle with
    position= new_position
  ; velocity= new_vol
  ; acceleration= new_acceleration
  ; stability_counter= final_stability_counter }

(* Web-based Visualization *)
let html_file = "/tmp/traffic_viz.html"

let data_file = "/tmp/traffic_data.json"

let create_html_file () =
  let html =
    {|<!DOCTYPE html>
<html>
<head>
  <title>2D Traffic Simulation</title>
  <style>
    body {
      margin: 0;
      padding: 20px;
      background: #1a1a1a;
      color: white;
      font-family: monospace;
    }
    canvas {
      border: 2px solid #444;
      display: block;
      margin: 20px auto;
      background: #282828;
    }
    #info {
      text-align: center;
      font-size: 16px;
      margin: 10px;
    }
  </style>
</head>
<body>
  <div id="info">Loading...</div>
  <canvas id="canvas" width="800" height="800"></canvas>
  <script>
    const canvas = document.getElementById('canvas');
    const ctx = canvas.getContext('2d');
    const info = document.getElementById('info');

    function drawSimulation() {
      fetch('traffic_data.json?' + new Date().getTime())
        .then(r => r.json())
        .then(data => {
          // Clear canvas
          ctx.fillStyle = '#282828';
          ctx.fillRect(0, 0, 800, 800);

          // Draw grid
          ctx.strokeStyle = '#333';
          ctx.lineWidth = 1;
          const gridSize = 50;
          for (let i = 0; i <= 800; i += gridSize) {
            ctx.beginPath();
            ctx.moveTo(i, 0);
            ctx.lineTo(i, 800);
            ctx.stroke();
            ctx.beginPath();
            ctx.moveTo(0, i);
            ctx.lineTo(800, i);
            ctx.stroke();
          }

          // Draw bicycles
          data.bicycles.forEach(b => {
            const x = b.position.x * 800 / data.world_size;
            const y = b.position.y * 800 / data.world_size;
            const speed = Math.sqrt(b.velocity.x ** 2 + b.velocity.y ** 2);
            const speedFactor = speed / data.max_speed;

            // Color based on speed (blue=slow, green=medium, red=fast)
            let r, g, blue;
            if (speedFactor < 0.5) {
              blue = Math.floor((1 - speedFactor * 2) * 255);
              g = Math.floor(speedFactor * 2 * 255);
              r = 0;
            } else {
              blue = 0;
              g = Math.floor((1 - (speedFactor - 0.5) * 2) * 255);
              r = Math.floor((speedFactor - 0.5) * 2 * 255);
            }
            ctx.fillStyle = `rgb(${r},${g},${blue})`;

            // Draw stability ring if counter > 0
            if (b.stability_counter > 0) {
              const stabilityFactor = b.stability_counter / 100.0;  // 100 is trigger
              const ringRadius = 6 + (stabilityFactor * 6);  // Grows from 6 to 12

              // Color: green -> yellow -> red as stability increases
              let ringR, ringG;
              if (stabilityFactor < 0.5) {
                ringR = Math.floor(stabilityFactor * 2 * 255);
                ringG = 255;
              } else {
                ringR = 255;
                ringG = Math.floor((1 - (stabilityFactor - 0.5) * 2) * 255);
              }

              ctx.strokeStyle = `rgb(${ringR},${ringG},0)`;
              ctx.lineWidth = 2;
              ctx.beginPath();
              ctx.arc(x, y, ringRadius, 0, Math.PI * 2);
              ctx.stroke();
            }

            // Draw circle
            ctx.beginPath();
            ctx.arc(x, y, 6, 0, Math.PI * 2);
            ctx.fill();

            // Draw velocity vector
            const angle = Math.atan2(b.velocity.y, b.velocity.x);
            const vx = x + Math.cos(angle) * speed * 0.5;
            const vy = y + Math.sin(angle) * speed * 0.5;

            ctx.strokeStyle = '#ffffff';
            ctx.lineWidth = 2;
            ctx.beginPath();
            ctx.moveTo(x, y);
            ctx.lineTo(vx, vy);
            ctx.stroke();

            // Draw arrow head
            ctx.beginPath();
            ctx.moveTo(vx, vy);
            ctx.lineTo(vx - 5 * Math.cos(angle - 0.5), vy - 5 * Math.sin(angle - 0.5));
            ctx.moveTo(vx, vy);
            ctx.lineTo(vx - 5 * Math.cos(angle + 0.5), vy - 5 * Math.sin(angle + 0.5));
            ctx.stroke();
          });

          // Update info
          info.textContent = `Step: ${data.step} | Bicycles: ${data.count} | Avg speed: ${data.avg_speed.toFixed(2)} | Press SPACE to add bicycles, 'q' to quit`;
        })
        .catch(e => {
          info.textContent = 'Waiting for simulation data...';
        });
    }

    setInterval(drawSimulation, 100);
    drawSimulation();
  </script>
</body>
</html>|}
  in
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
             b.id b.position.x b.position.y b.velocity.x b.velocity.y b.stability_counter )
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
  (* Start a simple HTTP server in the background *)
  let port = 8765 in
  let server_cmd =
    Printf.sprintf "cd /tmp && python3 -m http.server %d > /dev/null 2>&1 &"
      port
  in
  let _ = Sys.command server_cmd in
  Unix.sleepf 0.5 ;
  (* Give server time to start *)
  (* Open in browser *)
  let _ =
    Sys.command
      (Printf.sprintf "open http://localhost:%d/traffic_viz.html" port)
  in
  ()

(* Simulation step *)
let step model clock =
  let bicycles = grid_to_list model.grid in
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

let find_frame lst id = 
  let results = lst |> List.filter (fun b -> b.id = id) in
  match results with 
  | [] -> None
  | frame::_ -> Some frame

let handle_frame arrarr idx channel opt = 
  let value =
    match opt with
    | None -> 0.0
    | Some vec -> vec2sound vec
  in
  arrarr.(channel).(idx) <- value

let range_arr a b =
  let rec count n () =
    Seq.Cons(n,count (n + 1))
  in
  count a |> Seq.take b |> Array.of_seq

let record_frame idx model =
  if idx >= (rec_size - 1) then write_audio idx model.recording else () ;
  let lst = model.grid |> grid_to_list in
  range_arr 0 number_of_chans 
    |> Array.map (fun channel_number -> find_frame lst (channel_number + 10) |> handle_frame model.recording idx channel_number)


  

(* Main simulation loop with visualization *)
let rec simulate model n current_step =
  if n = 0 then model
  else (
    (if n mod 10 = 0 then
      visualize_web model current_step
    else
      ());
    if n mod 4000 = 0 then (
      print_int n ;
      print_endline "ok we are here" )
    else () ;
    (* Check for keyboard input *)
    let model_after_input =
      match check_input () with
      | Some ' ' -> add_bicycle model
      | Some 'q' ->
          Printf.printf "Quitting...\n" ;
          restore_terminal () ;
          exit 0
      | Some 'c' ->
          Printf.printf "Clear all\n" ;
          clear_bicycles model
      | Some 'r' ->
          Printf.printf "ok writing" ;
          write_audio (current_step - 1) model.recording ;
          model
      | _ -> model
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
