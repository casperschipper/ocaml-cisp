type bicycle = {id: int; position: float; velocity: float}

module IntMap = Map.Make (Int)

type grid = bicycle list IntMap.t

let rvf a b = 
  let low = min a b in
  let high = max a b in
  Random.float (high -. low) +. low

(* Convert grid back to a flat list of cars *)
let grid_to_list grid =
  IntMap.fold (fun _cell_idx cars acc ->
    cars @ acc
  ) grid []

let grid_to_sorted_list grid =
  IntMap.fold (fun _cell_idx cars acc ->
    cars @ acc
  ) grid []
  |> List.sort (fun c1 c2 -> Float.compare c1.position c2.position)

type grid_params = {cell_size: float}

let cell_index params bicycle =
  int_of_float (bicycle.position /. params.cell_size)

(* Build the grid from a list of bicycles *)
let build_grid params bicycles =
  List.fold_left
    (fun grid bicycle ->
      let idx = cell_index params bicycle in
      let cell_bicycles =
        match IntMap.find_opt idx grid with
        | None -> [bicycle]
        | Some bicycles -> bicycle :: bicycles
      in
      IntMap.add idx cell_bicycles grid )
    IntMap.empty bicycles

(* Get bicycles in current cell and next cell ahead *)
let get_nearby_bicycles grid params bicycle world_size =
  let idx = cell_index params bicycle in
  let max_idx = int_of_float (world_size /. params.cell_size) in
  let current_cell = Option.value (IntMap.find_opt idx grid) ~default:[] in
  let next_idx = if idx + 1 >= max_idx then 0 else idx + 1 in
  let next_cell = Option.value (IntMap.find_opt next_idx grid) ~default:[] in
  (* Also check the cell at the beginning if we're near the end (wrap-around) *)
  let wrap_cell =
    if idx >= max_idx - 1 then
      Option.value (IntMap.find_opt 0 grid) ~default:[]
    else []
  in
  current_cell @ next_cell @ wrap_cell

(* Find the closest bicycle ahead, accounting for wrap-around *)
let find_bicycle_ahead world_size bicycle nearby_bicycles =
  List.filter (fun other -> other.id <> bicycle.id) nearby_bicycles
  |> List.fold_left
       (fun acc other ->
         (* Calculate distance considering wrap-around *)
         let distance =
           if other.position >= bicycle.position then
             other.position -. bicycle.position
           else
             (* Wrapped around *)
             (world_size -. bicycle.position) +. other.position
         in
         match acc with
         | None -> Some (other, distance)
         | Some (best, min_dist) ->
             if distance < min_dist then Some (other, distance)
             else Some (best, min_dist) )
       None
  |> Option.map fst

type model =
  { grid: grid
  ; grid_params: grid_params
  ; desired_velocity: float
  ; safe_time_gap: float
  ; accel_rate: float
  ; decel_rate: float
  ; min_gap: float
  ; dt: float
  ; world_size: float
  ; next_id: int
  ; minimum_speed : float }

let mkBicycle id =
  {id; position= 0.1; velocity= 10.0}

let mkRandomBicycle world_size id = 
   {id; position= Random.float world_size; velocity= 10.0}

let mkBicycles world_size = List.init 20 (mkRandomBicycle world_size)

let init () =
  let pars = {cell_size= 10.0} in
  let world_size = 100.0 in
  { grid= build_grid pars (mkBicycles world_size)
  ; grid_params= pars
  ; desired_velocity= 10.0
  ; accel_rate= 10.0
  ; decel_rate= 10.0
  ; min_gap= 5.0
  ; dt= 1.0 /. 48.0
  ; world_size
  ; safe_time_gap= 0.2
  ; next_id= 10
  ; minimum_speed = 1.0 }

(* Calculate desired gap based on current velocity *)
let desired_gap model velocity =
  max model.min_gap (model.safe_time_gap *. velocity)

(* Update a single bicycle based on the bicycle ahead *)
let update_bicycle model bicycle bicycle_ahead_opt =
  let new_velocity =
    match bicycle_ahead_opt with
    | None ->
        (* No bicycle ahead - accelerate toward desired velocity *)
        if bicycle.velocity < model.desired_velocity then
          min
            (bicycle.velocity +. (model.accel_rate *. model.dt))
            model.desired_velocity
        else bicycle.velocity
    | Some ahead ->
        (* Calculate gap considering wrap-around *)
        let gap =
          if ahead.position >= bicycle.position then
            ahead.position -. bicycle.position
          else
            (model.world_size -. bicycle.position) +. ahead.position
        in
        let desired = desired_gap model bicycle.velocity in
        if gap > desired then
          (* Safe distance - can accelerate *)
          if bicycle.velocity < model.desired_velocity then
            min
              (bicycle.velocity +. (model.accel_rate *. model.dt))
              model.desired_velocity
          else bicycle.velocity
        else
          (* Too close - decelerate *)
          (* Emergency braking if gap is very small *)
          if gap < model.min_gap then
            max model.minimum_speed (bicycle.velocity -. (model.decel_rate *. model.dt *. 2.0))
          else
            let decel_amount =
              model.decel_rate *. model.dt *. (1.0 -. (gap /. desired))
            in
            max model.minimum_speed (bicycle.velocity -. decel_amount)
  in
  (* Update position based on velocity *)
  let new_position = mod_float (bicycle.position +. (new_velocity *. model.dt)) model.world_size in
  {bicycle with velocity= new_velocity; position= new_position}

  (* Visualization *)
let visualize_state model step =
  let bicycles = grid_to_sorted_list model.grid in
  let road_length = int_of_float model.world_size in
  let road = Array.make road_length '.' in

  (* Place bicycles on the road *)
  List.iter (fun bicycle ->
    let pos = int_of_float bicycle.position in
    if pos >= 0 && pos < road_length then
      road.(pos) <- 'B'
  ) bicycles;

  (* Print the visualization *)
  Printf.printf "\n=== Step %d ===\n" step;
  Printf.printf "|";
  Array.iter (fun c -> Printf.printf "%c" c) road;
  Printf.printf "|\n";

  (* Print statistics *)
  let avg_velocity =
    List.fold_left (fun sum b -> sum +. b.velocity) 0.0 bicycles
    /. float_of_int (List.length bicycles)
  in
  Printf.printf "Bicycles: %d | Avg velocity: %.2f\n"
    (List.length bicycles) avg_velocity;
  flush stdout

(* Web-based Visualization *)
let html_file = "/tmp/traffic_viz.html"
let data_file = "/tmp/traffic_data.json"

let create_html_file () =
  let html = {|<!DOCTYPE html>
<html>
<head>
  <title>Traffic Simulation</title>
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
  <canvas id="canvas" width="800" height="400"></canvas>
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
          ctx.fillRect(0, 0, 800, 400);

   

          // Draw bicycles
          data.bicycles.forEach(b => {
            const x = b.position * 800 / data.world_size;
            const speedFactor = b.velocity / data.desired_velocity;

            // Color based on speed
            let r, g;
            if (speedFactor < 0.5) {
              r = 255;
              g = Math.floor(speedFactor * 510);
            } else {
              r = Math.floor((1 - speedFactor) * 510);
              g = 255;
            }
            ctx.fillStyle = `rgb(${r},${g},0)`;

            // Draw circle
            ctx.beginPath();
            ctx.arc(x, 200, 8, 0, Math.PI * 2);
            ctx.fill();

            // Draw velocity indicator
            ctx.strokeStyle = '#ffffff';
            ctx.lineWidth = 2;
            ctx.beginPath();
            ctx.moveTo(x, 200);
            ctx.lineTo(x + b.velocity * 3, 200);
            ctx.stroke();
          });

          // Update info
          info.textContent = `Step: ${data.step} | Bicycles: ${data.count} | Avg velocity: ${data.avg_velocity.toFixed(2)} | Press SPACE to add bicycles, 'q' to quit`;
        })
        .catch(e => {
          info.textContent = 'Waiting for simulation data...';
        });
    }

    setInterval(drawSimulation, 100);
    drawSimulation();
  </script>
</body>
</html>|} in
  let oc = open_out html_file in
  output_string oc html;
  close_out oc

let visualize_web model step =
  let bicycles = grid_to_sorted_list model.grid in
  let avg_velocity =
    List.fold_left (fun sum b -> sum +. b.velocity) 0.0 bicycles
    /. float_of_int (List.length bicycles)
  in

  (* Create JSON data *)
  let json_bicycles = String.concat ",\n    "
    (List.map (fun b ->
      Printf.sprintf {|{"id": %d, "position": %.2f, "velocity": %.2f}|}
        b.id b.position b.velocity
    ) bicycles)
  in

  let json = Printf.sprintf {|{
  "step": %d,
  "count": %d,
  "avg_velocity": %.2f,
  "world_size": %.2f,
  "desired_velocity": %.2f,
  "bicycles": [
    %s
  ]
}|} step (List.length bicycles) avg_velocity model.world_size model.desired_velocity json_bicycles in

  (* Write JSON to file *)
  let oc = open_out data_file in
  output_string oc json;
  close_out oc

let setup_web_viz () =
  create_html_file ();
  (* Start a simple HTTP server in the background *)
  let port = 8765 in
  let server_cmd = Printf.sprintf "cd /tmp && python3 -m http.server %d > /dev/null 2>&1 &" port in
  let _ = Sys.command server_cmd in
  Unix.sleepf 0.5; (* Give server time to start *)
  (* Open in browser *)
  let _ = Sys.command (Printf.sprintf "open http://localhost:%d/traffic_viz.html" port) in
  ()

(* Simulation step *)
let step model =
  let bicycles = grid_to_list model.grid in
  let updated_bicycles = List.map (fun bicycle ->
    let nearby = get_nearby_bicycles model.grid model.grid_params bicycle model.world_size in
    let bicycle_ahead = find_bicycle_ahead model.world_size bicycle nearby in
    update_bicycle model bicycle bicycle_ahead
  ) bicycles in
  let new_grid = build_grid model.grid_params updated_bicycles in
  { model with grid = new_grid }

(* Add a new bicycle to the model *)
let add_bicycle model =
  let new_bicycle = mkBicycle model.next_id in
  let bicycles = grid_to_list model.grid @ [new_bicycle] in
  let new_grid = build_grid model.grid_params bicycles in
  { model with grid = new_grid; next_id = model.next_id + 1 }

let clear_bicycle model = 
  let new_bicycle = mkRandomBicycle model.world_size model.next_id in
  let new_grid = build_grid model.grid_params [new_bicycle] in
  { model with grid = new_grid; next_id = 10}

(* Terminal handling *)
let saved_term_attrs = ref None

let setup_terminal () =
  try
    let term_attrs = Unix.tcgetattr Unix.stdin in
    saved_term_attrs := Some term_attrs;
    let raw_attrs = { term_attrs with
      Unix.c_icanon = false;
      Unix.c_echo = false;
      Unix.c_vmin = 0;
      Unix.c_vtime = 0;
    } in
    Unix.tcsetattr Unix.stdin Unix.TCSANOW raw_attrs
  with
  | Unix.Unix_error _ -> () (* Terminal setup not supported, skip it *)

let restore_terminal () =
  match !saved_term_attrs with
  | Some attrs -> Unix.tcsetattr Unix.stdin Unix.TCSANOW attrs
  | None -> ()

(* Check for keyboard input without blocking *)
let check_input () =
  let (ready, _, _) = Unix.select [Unix.stdin] [] [] 0.0 in
  if ready = [] then None
  else
    let buf = Bytes.create 1 in
    let n = Unix.read Unix.stdin buf 0 1 in
    if n = 0 then None
    else Some (Bytes.get buf 0)

(* Main simulation loop with visualization *)
let rec simulate model n current_step =
  if n = 0 then model
  else begin
    (* visualize_state model current_step;  Text visualization *)
    visualize_web model current_step;    (* Web visualization *)

    (* Check for keyboard input *)
    let model_after_input =
      match check_input () with
      | Some ' ' ->
          add_bicycle model
      | Some 'q' ->
          Printf.printf "Quitting...\n";
          restore_terminal ();
          exit 0
      | Some 'c' -> 
          Printf.printf "clear all\n";
          clear_bicycle model
      | _ -> model
    in
    Unix.sleepf (1.0/.48.0);  (* Pause for 100ms between frames *)
    let new_model = step model_after_input in
    simulate new_model (n - 1) (current_step + 1)
  end

(* Run the simulation *)
let () =
  let model = init () in
  Printf.printf "Starting traffic simulation with %d bicycles\n"
    (List.length (grid_to_list model.grid));
  Printf.printf "Press SPACE to add bicycles, 'q' to quit\n";
  Printf.printf "Opening web visualization in browser...\n";
  flush stdout;
  setup_web_viz ();
  setup_terminal ();
  at_exit restore_terminal;
  let _final_model = simulate model 1_000_000_000 0 in
  restore_terminal ();
  ()