module S = Lo.Server
open Ants

let () = Process.sample_rate := 48000.0

let () =
  print_float !Process.sample_rate ;
  flush stdout

let record_duration = 5.0

let alpha = 1.4 (*  prefer paths with lots of pheromone *)

let beta = 1.0 (* prefer paths that are shorter *)

let num_nodes = 49

let n_side = num_nodes |> float_of_int |> sqrt |> int_of_float

let evaporation = 0.39

let exploration_bias = 0.0007

let deposit = 1.0

let num_ants = 10

let max_tour = 4

let brownian = 0.00002

let speed_of_comp = 20

let supercollider_entrydelay = 0.009

(* to update parameters over OSC and/or websocket while the audio thread is running *)
let buffer_mutex = Mutex.create ()

(* to protect nodes and distance_array from concurrent access *)
let nodes_mutex = Mutex.create ()

let shortest = ref 0.0

let update_position x y z_opt (Node old) =
  Node {id= old.id; x; y; z= Option.value ~default:0.0 z_opt; sync= Modified}

(** Represents a directed edge between two nodes in the ant colony graph.

    The edge stores both the distance between nodes and its inverse for efficient
    probability calculations in the ant colony optimization algorithm.

    Fields:
    - [start]: The starting node of the edge
    - [target]: The destination node of the edge
    - [dist]: The Euclidean distance between start and target nodes
    - [inv]: The inverse of the distance (1.0 / dist), used as a heuristic value;
             for zero-distance edges, this is set to 100.0 to avoid division by zero *)
type edge = Edge of {start: node; target: node; dist: float; inv: float}

type coordinate = Coordinate of {x: float; y: float; z: float}

type drawable_edge =
  | DrawableEdge of {start: coordinate; target: coordinate; weight: float}

type simple_edge = SimpleEdge of {start: int; target: int; weight: float}

let paths : node list list ref = ref []

let mkNode id x y z = Node {id; x; y; z; sync= Pristine}

(* |> Array.mapi (fun idx (x, y) -> mkNode idx x y) *)
(* [|(0.25, 0.25); (0.75, 0.25); (0.75, 0.75); (0.25, 0.75)|] *)

(** Function to generate an array of random points *)
let random_nodes () =
  let seed = 124 (*121*) |> Cisp.debugi "my random seed" in
  let seed = Random.int 12000 |> Cisp.debugi "myseed" in
  Spacegen.generate_random_points_3d ~seed ~count:num_nodes ~max_x:1.0
    ~max_y:1.0 ~max_z:0.0 ~f:mkNode

let grid_nodes () =
  Spacegen.generate_random_points_3d ~seed:124 ~count:49 ~max_x:1.0 ~max_y:1.0
    ~max_z:0.0 ~f:mkNode ~min_z:0.5

let from_traffic_points pts = Array.init

let nodes = grid_nodes ()

let distance (Node p1) (Node p2) =
  if p1.id = p2.id then 0.0
  else
    sqrt
      ( ((p1.x -. p2.x) ** 2.0)
      +. ((p1.y -. p2.y) ** 2.0)
      +. ((p1.z -. p2.z) ** 2.0) )

let get_inverse (Edge e) = e.inv

let mkEdge start target distance =
  Edge
    { start
    ; target
    ; dist= distance
    ; inv=
        ( match distance with
        | 0.0 -> 100.0
        | noneZero -> 1.0 /. noneZero ) }

type distance1d = Distance of edge Array.t Array.t

let debugi label i = print_string label ; print_int i ; print_newline ()

let generate_distance_matrix points =
  let n = Array.length points in
  Distance
    (Array.init n (fun i ->
         Array.init n (fun j ->
             let dist = distance points.(i) points.(j) in
             mkEdge points.(i) points.(j) dist ) ) )

let update_distance_matrix_inplace points (Distance matrix) =
  let n = Array.length points in
  let matrix_size = Array.length matrix in
  if n <> matrix_size then
    Error
      (Printf.sprintf
         "Size mismatch: points array has %d elements but matrix has %d rows" n
         matrix_size )
  else if Array.exists (fun row -> Array.length row <> n) matrix then
    Error "Matrix is not square or has inconsistent row sizes"
  else (
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        let dist = distance points.(i) points.(j) in
        matrix.(i).(j) <- mkEdge points.(i) points.(j) dist
      done
    done ;
    Ok () )

let get_distance i j (Distance dists) =
  let line = dists.(i) in
  line.(j)

let get_line i dists = Array.init num_nodes (fun j -> get_distance i j dists)

let pretty_print_row row =
  (* Convert each float in the row to a string with specified formatting *)
  let row_string =
    Array.map (fun (Edge e) -> e.dist |> Printf.sprintf "%.2f") row
  in
  (* Join the formatted strings with spaces and print the row *)
  Printf.printf "[ %s ]\n" (String.concat "; " (Array.to_list row_string))

(* Function to pretty print a 2D array of floats *)
let pretty_print_matrix arr =
  (* Iterate over each row in the 2D array *)
  print_endline "-*-" ;
  Array.iter pretty_print_row arr ;
  print_endline "-*-"

let update_matrix_2 point_index new_point points (Distance dist) =
  if point_index >= 0 && point_index < Array.length points then (
    points.(point_index) <- new_point ;
    for i = 0 to num_nodes - 1 do
      if i <> point_index then (
        let dist_val = distance new_point points.(i) in
        let edge_forward = mkEdge new_point points.(i) dist_val in
        let edge_backward = mkEdge points.(i) new_point dist_val in
        dist.(point_index).(i) <- edge_forward ;
        dist.(i).(point_index) <- edge_backward )
    done ;
    Distance dist (* ; pretty_print_matrix dist *) )
  else Distance dist

(* Thread-safe version that locks the nodes_mutex *)
let update_matrix_2_safe point_index new_point points dist =
  Mutex.lock nodes_mutex ;
  let result = update_matrix_2 point_index new_point points dist in
  Mutex.unlock nodes_mutex ; result

(*
  0    1   2
0     0,1
1 1,0 1,1 1,2
2     2,1

*)

let distance_array = generate_distance_matrix nodes

let mkPheromonesArr = Array.make_matrix num_nodes num_nodes 0.0

let duplicateArrArr arrarr = Array.map Array.copy arrarr

type controllers =
  { alpha: float
  ; beta: float
  ; deposit: float
  ; evaporation: float
  ; num_ants: int
  ; exploration_bias: float
  ; max_tour: int
  ; brownian: float
  ; speed_of_comp: int
  ; supercollider_entrydelay: float }

let controllers_to_string ctr =
  Printf.sprintf
    "Controllers {\n\
    \  alpha: %.2f\n\
    \  beta: %.2f\n\
    \  deposit: %.2f\n\
    \  evaporation: %.2f\n\
    \  num_ants: %d\n\
    \  max_tour: %d\n\
    \  brownian: %.2f\n\
    \  speed_of_comp: %d\n\
    \  supercollider_entrydelay: %.3f\n\
     }"
    ctr.alpha ctr.beta ctr.deposit ctr.evaporation ctr.num_ants ctr.max_tour
    ctr.brownian ctr.speed_of_comp ctr.supercollider_entrydelay

type indexed_point = IndexedPoint of {idx: int; x: float; y: float; z: float}

type ctrlMsg =
  | Alpha of float
  | Beta of float
  | Deposit of float
  | Evaporation of float
  | Exploration of float
  | NumAnts of int
  | MaxTour of int
  | Brownian of float
  | SpeedOfComp of int
  | ResetPoints of int
  | Supercollider_Entrydelay of float

type oscMessage =
  | CtrlUpdate of ctrlMsg
  | WriteHistory
  | MovePoint of indexed_point
  | MoveAllPoints of (float * float) Array.t
  | Noop

let update_ctrl msg ctrl =
  match msg with
  | Alpha x -> {ctrl with alpha= x}
  | Beta x -> {ctrl with beta= x}
  | Deposit d -> {ctrl with deposit= d}
  | NumAnts na -> {ctrl with num_ants= na}
  | Evaporation eva -> {ctrl with evaporation= eva}
  | Exploration exp -> {ctrl with exploration_bias= exp}
  | MaxTour mt -> {ctrl with max_tour= mt}
  | Brownian brown -> {ctrl with brownian= brown}
  | SpeedOfComp sp -> {ctrl with speed_of_comp= sp}
  | ResetPoints x -> ctrl
  | Supercollider_Entrydelay dt -> {ctrl with supercollider_entrydelay= dt}

let initial =
  { alpha
  ; beta
  ; deposit
  ; evaporation
  ; num_ants
  ; exploration_bias
  ; max_tour
  ; brownian
  ; speed_of_comp
  ; supercollider_entrydelay }

let set_alpha a ctrl = {ctrl with alpha= a}

let set_beta b ctrl = {ctrl with beta= b}

let set_deposit d ctrl = {ctrl with deposit= d}

let set_evaporation e ctrl = {ctrl with evaporation= e}

let set_ants a ctrl = {ctrl with num_ants= a}

let set_brownian a ctrl = {ctrl with brownian= a}

let set_supercollider_entrydelay a ctrl = {ctrl with supercollider_entrydelay= a}

let set_max_tour a ctrl =
  let mx = if a > 1 then a else 1 in
  {ctrl with max_tour= mx}

let set_exploration_bias a ctrl = {ctrl with exploration_bias= a}

let current_buffer = ref initial

let next_buffer = ref initial

let pint label i =
  print_string (label ^ " ") ;
  print_int i ;
  print_newline () ;
  i

let pflt label f =
  print_string (label ^ " ") ;
  print_float f ;
  print_newline () ;
  f

let swap_buffers () =
  Mutex.lock buffer_mutex ;
  let temp = !current_buffer in
  current_buffer := !next_buffer ;
  next_buffer := temp ;
  Mutex.unlock buffer_mutex

let handle_float_arg datas =
  let data = Array.to_list datas in
  match data with
  | [`Float f]
   |[`Double f] ->
      Some (f : float)
  | _ -> None

let handle_int_float_float datas =
  let data = Array.to_list datas in
  match data with
  | [`Int32 idx; `Float x; `Float y; `Float z] ->
      Some (IndexedPoint {idx; x; y; z})
  | _ -> None

exception Parse_failure

let partition_and_parse_imperative arr =
  let parse x =
    match x with
    | `Float f -> Some f
    | _ -> None
  in
  let n = Array.length arr / 2 in
  (* let _ = Printf.printf "received OSC message vec2 array of size %d" n in *)
  let result = Array.make n (0.0, 0.0) in
  try
    for i = 0 to n - 1 do
      let x =
        match parse arr.(2 * i) with
        | Some v -> v
        | None -> raise Parse_failure
      in
      let y =
        match parse arr.((2 * i) + 1) with
        | Some v -> v
        | None -> raise Parse_failure
      in
      result.(i) <- (x, y)
    done ;
    Some result
  with
  | Parse_failure -> None

let handle_float_array dts =
  (* Printf.printf "hi seems ok %d" (Array.length dts); *)
  if Array.length dts = 98 then partition_and_parse_imperative dts
  else (
    print_endline "array of unexpected size" ;
    None )

let handle_int_arg datas =
  let data = Array.to_list datas in
  match data with
  | [`Int32 i]
   |[`Int64 i] ->
      Some (i : int)
  | _ -> None

let update_and_swap ~update value =
  Mutex.lock buffer_mutex ;
  next_buffer := update value !current_buffer ;
  current_buffer := !next_buffer ;
  Mutex.unlock buffer_mutex

(* let update_distance_matrix nodes old_distances index new_point =
   let copy = Array.copy old_distances in
   nodes.(index) <- new_point;
   Seq.ints 0 |> Seq.take num_nodes |> Seq.map (fun idx ->
     let new_dist = distance new_point nodes.(idx) in
     Toolkit.update_at_index old_distances idx (fun _ -> new_dist)) *)

let update_points opt_ipoint =
  match opt_ipoint with
  | Some (IndexedPoint {idx; x; y; z}) ->
      if idx < num_nodes && idx >= 0 then
        ignore
          (update_matrix_2_safe idx (mkNode idx x y z) nodes distance_array)
      else ()
  | None -> ()

exception Incorrect_Points

let update_all_points pts =
  let n = Array.length pts in
  let fst (x, _) = x in
  let snd (_, y) = y in
  if n != 49 then raise Incorrect_Points else Mutex.lock nodes_mutex ;
  for i = 0 to n - 1 do
    nodes.(i) <-
      mkNode i
        (fst pts.(i))
        (snd pts.(i))
        0.0 (* divide by three because it is 300 instead of 100 *)
  done ;
  ignore (update_distance_matrix_inplace nodes distance_array) ;
  Mutex.unlock nodes_mutex

let reset_points arg_number = ()


let parse_osc_message path data =
  let withDefault f arg = arg |> Option.map f |> Option.value ~default:Noop in
  match path with
  | "/alpha" ->
      data |> handle_float_arg |> withDefault (fun a -> CtrlUpdate (Alpha a))
  | "/beta" ->
      data |> handle_float_arg |> withDefault (fun b -> CtrlUpdate (Beta b))
  | "/evaporation" ->
      data |> handle_float_arg
      |> withDefault (fun e -> CtrlUpdate (Evaporation e))
  | "/deposit" ->
      data |> handle_float_arg |> withDefault (fun d -> CtrlUpdate (Deposit d))
  | "/num_ants" ->
      data |> handle_int_arg |> withDefault (fun n -> CtrlUpdate (NumAnts n))
  | "/point" ->
      data |> handle_int_float_float |> withDefault (fun p -> MovePoint p)
  | "/all_points" ->
      data |> handle_float_array |> withDefault (fun pts -> MoveAllPoints pts)
  | "/exploration_bias" ->
      data |> handle_float_arg
      |> withDefault (fun e -> CtrlUpdate (Exploration e))
  | "/max_tour" ->
      data |> handle_int_arg |> withDefault (fun m -> CtrlUpdate (MaxTour m))
  | "/brownian" ->
      data |> handle_float_arg |> withDefault (fun b -> CtrlUpdate (Brownian b))
  | "/write_history" -> WriteHistory
  | "/speed_of_comp" ->
      data |> handle_int_arg
      |> withDefault (fun s -> CtrlUpdate (SpeedOfComp s))
  | "/supercollider_entrydelay" ->
      data |> handle_float_arg |> withDefault (fun sced -> CtrlUpdate (Supercollider_Entrydelay sced))
  | _ -> Noop

let handle_osc_message_parse finish_history_callback path data =
  let msg = parse_osc_message path data in
  match msg with
  | MovePoint p -> update_points (Some p)
  | WriteHistory -> finish_history_callback ()
  | CtrlUpdate ctrl -> update_and_swap ~update:update_ctrl ctrl
  | Noop -> ()
  | MoveAllPoints pts -> update_all_points pts

let osc_thread_function write_history_callback () =
  let server =
    S.create 57666 (handle_osc_message_parse write_history_callback)
  in
  while true do
    S.recv server ;
    (* print_endline "osc" ; *)
    swap_buffers ()
    (* Swap buffers at a safe point, can be adjusted based on needs *)
  done

let print_sync sync =
  match sync with
  | Pristine -> "pristine"
  | Modified -> "modified"
  | Synced -> "synced"

let print_node = function
  | Node {id; x; y; z; sync} ->
      let sync = print_sync sync in
      Printf.printf "Node {\n" ;
      Printf.printf "  id: %d\n" id ;
      Printf.printf "  x: %.2f\n" x ;
      Printf.printf "  y: %.2f\n" y ;
      Printf.printf "  z: %.2f\n" z ;
      Printf.printf "  sync: %s" sync ;
      Printf.printf "}\n"

let get_node_id (Node n) = n.id

let get_node_x (Node n) = n.x

let get_node_y (Node n) = n.y

let get_node_z (Node n) = n.z

let default_node = Node {id= 0; x= 0.0; y= 0.0; z= 0.0; sync= Synced}

(* type dist_arr = float Array.t Array.t *)

module IndexSet = Set.Make (Int)

(* type walk = Walk of { steps : int list; total_dist : float } *)

let get_dist (Edge e) = e.dist

let get_target (Edge e) = e.target

let get_start (Edge e) = e.start

let getDist (Edge e) = e.dist

(* let sort_by_distance arr =
   Array.sort
     (fun a b ->
       let da, db = (getDist a, getDist b) in
       if da == db then 0 else if a < b then 1 else -1)
     arr;
   arr *)

let identity x = x

type state =
  | State of
      { current_ant: int
      ; current: node
      ; targets: node list
      ; visited: node list
      ; visited_edges: edge list
      ; total_dist: float
      ; best_dist: float
      ; n_visited: int }

let get_node_id (Node n) = n.id

let get_node_x (Node n) = n.x

let print_edge (Edge edge) =
  print_string "start: " ;
  print_node edge.start ;
  print_string "target: " ;
  print_node edge.target ;
  print_string "dist" ;
  print_float edge.dist ;
  print_string "inverse " ;
  print_float edge.inv ;
  print_newline ()

let pretty_print_distance (Distance arr) =
  Array.iter (Array.iter print_edge) arr

let print_state = function
  | State
      { current_ant
      ; current
      ; targets
      ; visited
      ; visited_edges
      ; total_dist
      ; best_dist
      ; _ } ->
      Printf.printf "State {\n" ;
      Printf.printf "  current_ant: %d\n" current_ant ;
      Printf.printf "  current: %d\n" (get_node_id current) ;
      (* Assuming current is a node, change if necessary *)
      print_string "  targets: %s\n" ;
      List.iter print_node targets ;
      print_string "  visited: %s\n" ;
      List.iter print_node visited ;
      print_string "  visited_edges: %s\n" ;
      List.iter print_edge visited_edges ;
      Printf.printf "  total_dist: %.2f\n" total_dist ;
      Printf.printf "  best_dist: %.2f\n" best_dist ;
      Printf.printf "}\n"

let get_coords (Node {x; y; z; _}) = Coordinate {x; y; z}

let logflt label f =
  print_string label ; print_string " = " ; print_float f ; print_newline ()

let logint label i =
  print_string label ; print_string " = " ; print_int i ; print_newline ()

let play_stream stream =
  (* Create a reference to keep track of the call count *)
  let stream = ref stream in
  fun () ->
    let new_value = Cisp.head !stream |> Option.value ~default:0.0 in
    let _ =
      stream := Cisp.tail !stream |> Option.value ~default:(Cisp.st 0.0)
    in
    new_value

(* Define a function that takes another function `f` and returns a new function that calls `f` once every 1000 times *)
(**
let with_throttled_execution n f =
  (* Create a reference to keep track of the call count *)
  let call_count = ref 0 in
  fun () ->
    (* Increment the call count each time the function is called *)
    (* Check if the call count is a multiple of 1000 *)
    if !call_count >= n then
      (* Call the function `f` *)
      let _ = call_count := 0 in
      f ()
    else incr call_count ;
    (* Do nothing otherwise *)
    ()
    *)

let deposit_pher phero_arr tour pheromone =
  List.iter
    (fun (Edge e) ->
      let start_id = get_node_id e.start in
      let target_id = get_node_id e.target in
      let old_value = phero_arr.(start_id).(target_id) in
      phero_arr.(start_id).(target_id) <- pheromone +. old_value )
    tour

(* let pher_from_distance total_dist = 1.0 /. total_dist *)
let get_pheromone p_arr start target = p_arr.(start).(target)

let arr_map_inplace f arr =
  (* destructive map *)
  for i = 0 to Array.length arr - 1 do
    arr.(i) <- f arr.(i)
  done

let clipf mini maxi input = max mini input |> min maxi

let evaporate p_arr =
  for i = 0 to Array.length p_arr - 1 do
    for j = 0 to Array.length p_arr.(i) - 1 do
      let old_p_arr = p_arr.(i).(j) in
      let clipped = Toolkit.clipf 0.0 1.0 !current_buffer.evaporation in
      p_arr.(i).(j) <- old_p_arr *. (1.0 -. clipped)
    done
  done

let calculate_entropy p_arr =
  (* Calculate total pheromone sum *)
  let total_pheromone = Array.fold_left ( +. ) 0.0 p_arr in
  (* Calculate entropy *)
  let entropy =
    Array.fold_left
      (fun acc pheromone ->
        if pheromone > 0.0 then
          if total_pheromone > 0.0 then
            let probability = pheromone /. total_pheromone in
            acc -. (probability *. log probability)
          else acc
        else acc )
      0.0 p_arr
  in
  entropy

type 'a debug_state = {results: 'a list; count: int}

let debug_state = ref {results= []; count= 0}

let debug_sequence pher_arr seq =
  let debug_f x =
    let state = !debug_state in
    if state.count >= 300 then (
      let entropy = calculate_entropy pher_arr in
      print_string "entropy of row 0 is now: " ;
      print_float entropy ;
      print_newline () ;
      debug_state := {results= entropy :: state.results; count= 0} ;
      x )
    else
      let _ = debug_state := {results= state.results; count= state.count + 1} in
      x
  in
  seq |> Seq.map debug_f

type weighted_edge = {edge: edge; weight: float}

let sometimes_random =
  let open Cisp in
  play_stream
    ( Cisp.rvf (Cisp.st 0.0) (Cisp.st 1.0)
    |> Cisp.hold (Cisp.rv (st 10) (st 100000)) )

let always_random max = Random.float max

type 'a weighted = {w: float; item: 'a}

let pick_weighted lst =
  let total_weight = List.fold_left (fun acc x -> x.w +. acc) 0.0 lst in
  let p = Random.float 1.0 in
  let rec aux current_sum remaining =
    match remaining with
    | [] -> None
    | [x] -> Some x.item
    | x :: xs ->
        let next = (x.w /. total_weight) +. current_sum in
        if p >= next then aux next xs else Some x.item
  in
  aux 0.0 lst

let select_next_edge_new pher_arr allowed_edges =
  let p_combi =
    Array.fold_left
      (fun acc edge ->
        let start = edge |> get_start |> get_node_id in
        let target = edge |> get_target |> get_node_id in
        let pher_level = get_pheromone pher_arr start target in
        let heuristic = get_inverse edge in
        let weight =
          (pher_level ** !current_buffer.alpha)
          *. (heuristic ** !current_buffer.beta)
          +. !current_buffer.exploration_bias
        in
        {w= weight; item= edge} :: acc )
      [] allowed_edges
  in
  match pick_weighted p_combi with
  | Some edge -> edge
  | None -> raise (Failure "the allowed edges array was empty")

let print_edge (Edge e) =
  print_string "printing edge: " ;
  print_string "e.dist - " ;
  print_float e.dist ;
  print_string " " ;
  print_string "e.start .id - " ;
  print_int (get_node_id e.start) ;
  print_string " " ;
  print_string "e.target .id - " ;
  print_int (get_node_id e.target) ;
  print_string "\n"

let print_nodes label nodes =
  print_string label ;
  List.iter
    (fun x ->
      print_int (get_node_id x) ;
      print_char ' ' )
    nodes

let debug_edges label edges =
  print_endline label ;
  List.iter
    (fun (Edge e) ->
      print_int (get_node_id e.target) ;
      print_char ' ' )
    edges ;
  print_newline () ;
  edges

let remove_node node lst_nodes =
  List.filter (fun n -> get_node_id n != get_node_id node) lst_nodes

let print_int_lst label ilist =
  print_string (label ^ " ") ;
  List.iter (fun i -> print_int i ; print_char ' ') ilist ;
  print_newline ()

let get_array_value arr index =
  if index >= 0 && index < Array.length arr then Some arr.(index)
    (* Return Some(value) if the index is within bounds *)
  else None (* Return None if the index is out of bounds *)

(*
 * complete nodes are all the nodes that are there in the original configuration
 *)
let add_missing_link edges =
  let missing =
    match edges with
    | [] -> None
    | x :: _ -> (
      match List.rev edges with
      | y :: _ ->
          let start = get_start x in
          let target = get_target y in
          Some (mkEdge (get_target y) (get_start x) (distance start target))
      | [] -> None )
  in
  match missing with
  | None -> []
  | Some x -> x :: edges

let start_new pheromones complete_nodes (State state) =
  let _ =
    match state.visited_edges with
    | [] -> print_string "first time no deposit"
    | vstd ->
        deposit_pher pheromones vstd
          (!current_buffer.deposit /. state.total_dist)
  in
  (* reset, all targets become available again *)
  (* let _ =
       print_endline "these are the nodes on reset ";
       print_nodes "state visited (reset): " state.visited;
       print_newline ()
     in *)
  let first_pick =
    (* choose a random starting point *)
    (* let _ =
         if Array.length reset_targets == 0 then print_endline "OH NO!"
         else print_endline "ok"
       in *)
    (* nodes
       |> Toolkit.flip get_array_value 0
       |> Option.value ~default:default_node *)
    nodes |> Toolkit.choice_arr_opt |> Option.value ~default:default_node
  in
  let updated_ant =
    if state.current_ant >= !current_buffer.num_ants then
      let _ = ignore (evaporate pheromones) in
      0
    else
      (* let _ = logint "current ant : " state.current_ant in *)
      state.current_ant + 1
  in
  let best_dist = min state.best_dist state.total_dist in
  let () = shortest := best_dist in
  let () =
    if state.total_dist < state.best_dist then (
      print_string "print path" ;
      List.iter
        (fun i -> print_int i ; print_char ' ')
        (List.map get_node_id state.visited) ;
      print_newline () ;
      print_string "smallest is now: " ;
      print_float state.total_dist ;
      print_newline () ;
      paths := state.visited :: Toolkit.lst_take 10 !paths ;
      print_string "size of paths" ;
      print_int (List.length !paths) ;
      print_newline () )
    else ()
  in
  State
    { current_ant= updated_ant
    ; current= first_pick
    ; targets= complete_nodes
    ; visited= []
    ; total_dist= 0.0
    ; visited_edges= []
    ; best_dist
    ; n_visited= 0 }

let pretty_print_flt_row row =
  let row_string = Array.map (fun flt -> Printf.sprintf "%.2f " flt) row in
  Printf.printf " [ %s ]\n" (String.concat "; " (Array.to_list row_string))

let pretty_print_floats arr = Array.iter pretty_print_flt_row arr

let debug_phers pheromones () =
  print_string "pher :" ;
  pretty_print_floats pheromones ;
  print_float (calculate_entropy pheromones.(2)) ;
  print_newline ()

let throttled_pher_func pheromones =
  Cisp.with_throttled_execution 44100 (debug_phers pheromones)

let pick_next_point pheromones original_nodes dist_matrix (State state) =
  if state.n_visited >= !current_buffer.max_tour then
    start_new pheromones original_nodes (State state)
  else
    match state.targets with
    | [] -> start_new pheromones original_nodes (State state)
    | _ :: _ ->
        (* let _ =
            print_string "current = ";
            print_int (get_node_id state.current);
            print_endline ""
          in *)
        (* let size = Array.length dist_matrix in *)
        (* current should be within bounds *)
        let edges = get_line (get_node_id state.current) dist_matrix in
        (* let _ = Array.iter print_edge edges in *)
        (* remove current point from targets *)
        let new_targets = remove_node state.current state.targets in
        (* let new_targets = state.targets in *)
        (* let () = print_nodes "new targets: " new_targets in *)
        (* get the target node ids *)
        let ids = new_targets |> List.map get_node_id in
        let filtered_edges =
          (* only get the edges that are of targets *)
          edges |> Array.to_list
          |> List.filter (fun (Edge e) -> List.mem (get_node_id e.target) ids)
          (* |> debug_edges "filtered edges targets" *)
          |> Array.of_list
        in
        if Array.length filtered_edges < 1 then
          start_new pheromones original_nodes (State state)
        else
          let picked_edge = select_next_edge_new pheromones filtered_edges in
          (* let _ = throttled_pher_func () in *)
          (* let _ = debug_edges "filtered edges" (filtered_edges |> Array.to_list) in *)
          let new_target = picked_edge |> get_target in
          (* let () =
              print_endline "\n visited ";
              print_nodes "old visited state" state.visited;
              print_endline "\n"
            in *)
          State
            { current_ant= state.current_ant
            ; current= new_target
            ; targets= new_targets
            ; visited= state.current :: state.visited
            ; visited_edges= picked_edge :: state.visited_edges
            ; total_dist= state.total_dist +. get_dist picked_edge
            ; best_dist= state.best_dist
            ; n_visited= state.n_visited + 1 }

(* let get_x (Node n) = n.x *)
let read_file filename =
  let result = ref "" in
  let in_channel = open_in filename in
  try
    while true do
      let line = input_line in_channel in
      result := !result ^ line ^ "\n"
    done ;
    !result
  with
  | End_of_file -> close_in in_channel ; !result

(* end of ant part *)

module SuperAnts = struct
  (* The SuperCollider default address and port *)
  let default_host = "127.0.0.1"

  let default_port = 57110

  (* Initialize an OSC address *)
  let address = Lo.Address.create default_host default_port

  (* Send a synth event to SuperCollider *)
  let send_synth ?(synth_id = -1) ?(add_action = 1) ?(target_id = 0) ~synth_name
      ~freq ~range ~imp ~q ~releaseDur ~pan () =
    Lo.send address "/s_new"
      [ `String synth_name
      ; `Int32 synth_id
      ; `Int32 add_action
      ; `Int32 target_id
      ; `String "freq"
      ; `Float freq
      ; `String "range"
      ; `Float range
      ; `String "imp"
      ; `Float imp
      ; `String "q"
      ; `Float q
      ; `String "releaseDur"
      ; `Float releaseDur
      ; `String "pan"
      ; `Float pan ]

  let ant_crackle ~freq ~range ~imp ~q ~releaseDur ~pan () =
    send_synth ~synth_name:"antCrackle" ~freq ~range ~imp ~q ~releaseDur ~pan ()
end

let signal pheromones () =
  (* produces node indexes as ints *)
  let nodes_list = nodes |> Array.to_list in
  let init =
    State
      { current_ant= 0
      ; current= Array.get nodes 0
      ; targets= nodes_list
      ; visited= []
      ; total_dist= 0.0
      ; visited_edges= []
      ; best_dist= 1000000.0
      ; n_visited= 0 }
  in
  let update = pick_next_point pheromones nodes_list distance_array in
  let eval (State state) = get_node_id state.current in
  Cisp.simpleRecursive init update eval

let line_table =
  let steps = num_nodes in
  let one_step = 1.0 /. float_of_int steps in
  let table = List.init steps (fun i -> float_of_int i *. one_step) in
  table |> Array.of_list

let lookup input = input |> Seq.map (fun idx -> Array.get line_table idx)

let output pheromones = signal pheromones () |> lookup

let only_compute pheromones = signal pheromones () |> Seq.map (fun _ -> ())

(* let () =
   pretty_print_matrix distance_array;
   print_endline "fish";
   let () = signal () |> Seq.take 100000 |> Seq.iter ignore in
   (* |> List.iter (fun x ->
          print_int x;
          print_string " "); *)
   flush stdout *)

let otherSignal pheromones () =
  let open Cisp in
  let c = st () in
  let u () model =
    let phers = Array.get pheromones model in
    let weighted =
      Array.mapi (fun idx item -> {w= item; item= idx}) phers |> Array.to_list
    in
    let next_pher = pick_weighted weighted in
    match next_pher with
    | Some i -> i
    | None -> 0
  in
  let eval inp =
    let scale x = (x /. float_of_int num_nodes *. 1024.0) +. 64. |> mtof in
    line_table.(inp) |> scale
  in
  Cisp.recursive c 0 u eval |> hold (st 30) |> sinosc2

let nodesStream pheromones () =
  let open Cisp in
  let random_index () = Toolkit.rvi 0 num_nodes in
  let c = st () in
  let u () model =
    let phers = Array.get pheromones model in
    let weighted =
      Array.mapi (fun idx item -> {w= item; item= idx}) phers |> Array.to_list
    in
    let next_pher = pick_weighted weighted in
    match next_pher with
    | Some i -> i
    | None -> random_index ()
  in
  let eval inp = nodes.(inp) in
  let init = random_index () in
  Cisp.recursive c init u eval

let nodesStreamNoisy pheromones () =
  let open Cisp in
  let random_index () = Toolkit.rvi 0 num_nodes in
  let c = st () in
  let u () (model, i) =
    if i > 441000 then (random_index (), 0)
    else
      let phers = Array.get pheromones model in
      let weighted =
        Array.mapi (fun idx item -> {w= item; item= idx}) phers |> Array.to_list
      in
      let next_pher = pick_weighted weighted in
      match next_pher with
      | Some next_id -> (next_id, i + 1)
      | None -> (random_index (), 0)
  in
  let eval (inp, _) = nodes.(inp) in
  let init = (random_index (), 0) in
  Cisp.recursive c init u eval

let play_node_stereo (Node node) = (node.x -. 0.5, node.y -. 0.5)

(* this creates a stereo signal, navigating through the pheromones only *)
(* the repeats allows for different update rates , where 1 is sr *)
let justThePath pheromones repeats =
  nodesStreamNoisy pheromones ()
  |> Cisp.hold (Cisp.st repeats)
  |> Seq.map play_node_stereo |> Cisp.unzip

let just_the_path_from_nodes nodes_sq repeats =
  nodes_sq
  |> Cisp.hold (Cisp.st repeats)
  |> Seq.map play_node_stereo |> Cisp.unzip

let node_scalar n =
  sqrt (get_node_x n ** 2.0) *. (get_node_y n ** 2.0) *. (get_node_z n -. 0.5)

(* let only_noisy_paths pheromones =
  let f viable_threshold acc ph =
    if ph > viable_threshold then acc else if ph > 0.1 then 1 + acc else acc
  in
  let get_att (Node node) =
    node.id
    |> fun idx ->
    pheromones.(idx)
    |> Array.fold_left (f !current_buffer.viable_threshold) 0
    |> fun bins_with_value ->
    Cisp.linlin 0.0 49.0 0.0 7.0 (float_of_int bins_with_value)
    |> Toolkit.clipf 0.0 1.0
  in
  nodesStream pheromones ()
  |> Seq.map (fun node -> get_att node *. node_scalar node)
(* let open Cisp in
  let random_index () = Toolkit.rvi 0 num_nodes in
  let c = st () in
  let u () (model, t) =
    let phers = Array.get pheromones model in
    let weighted =
      Array.mapi (fun idx item -> {w= item; item= idx}) phers |> Array.to_list
    in
    let next_pher = pick_weighted weighted in
    let new_time = t + 1 in
    let ix = if t > 100000 then random_index () else model in
    (* TODO think about the 10000 as a parameter *)
    match next_pher with
    | Some i -> (i, ix)
    | None -> (random_index (), 0)
  in
  let eval (inp, _) =
    nodes.(inp) |> fun (Node node) -> (node.x -. 0.5, node.y -. 0.5)
  in
  let init = (random_index (), 0) in
  Cisp.recursive c init u eval |> Cisp.hold (st repeats) |> Seq.unzip *)
  *)

(* this turns the segments into points that we can render in a csound score *)
type point = {x: float; y: float; z: float; t: float}

(* let distance a b =
  let dx = a.x -. b.x in
  let dy = a.y -. b.y in
  let dz = a.z -. b.z in
  sqrt (dx *. dx +. dy *. dy +. dz *. dz) *)

let node_to_point_seq (scale : float) (speed : float) (nodes : node Seq.t) :
    point Seq.t =
  let rec aux prev rest () =
    match (prev, rest ()) with
    | Some (Node a), Seq.Cons (Node b, tl) ->
        let dist = distance (Node a) (Node b) in
        let t = dist /. speed in
        Seq.Cons ({x= a.x; y= a.y; z= a.z; t}, aux (Some (Node b)) tl)
    | Some (Node a), Seq.Nil ->
        Seq.Cons ({x= a.x; y= a.y; z= a.z; t= 0.0}, fun () -> Seq.Nil)
    | None, Seq.Cons (Node b, tl) -> aux (Some (Node b)) tl ()
    | None, Seq.Nil -> Seq.Nil
  in
  aux None nodes

(* let justThePathButLines =
  let interpolate_segment ~start ~stop ~samples =
    let step = (stop -. start) /. float_of_int samples in
    Seq.init samples (fun i -> start +. float_of_int i *. step)
  in

  let waveform_of_targets ~sample_rate target_values segment_lengths =
  (* Create pairs of (start, stop, segment_duration) *)
    let rec make_segments tvs sls =
      match Seq.uncons tvs, Seq.uncons sls with
      | Some (v1, tvs'), Some (len, sls') ->
          begin match Seq.uncons tvs' with
          | Some (v2, rest) ->
              let samples = int_of_float (len *. sample_rate) in
              let seg = interpolate_segment ~start:v1 ~stop:v2 ~samples in
              Seq.append seg (make_segments (Seq.cons v2 rest) sls')
          | None -> Seq.empty
          end
      | _ -> Seq.empty
    in
    make_segments target_values segment_lengths *)

(* let nodesStream pheromones () =
  let open Cisp in
  let random_index () = Toolkit.rvi 0 num_nodes in
  let c = st () in
  let u () (model, t) =
    let phers = Array.get pheromones model in
    let weighted =
      Array.mapi (fun idx item -> {w= item; item= idx}) phers |> Array.to_list
    in
    let next_pher = pick_weighted weighted in
    let new_time = t + 1 in
    let ix, nt =
      if new_time > 100000 then (random_index (), 0) else (model, 0)
    in
    (* TODO think about the 10000 as a parameter *)
    match next_pher with
    | Some i -> (i, nt)
    | None -> (random_index (), nt)
  in
  let eval (inp, _) = nodes.(inp) in
  let init = (random_index (), 0) in
  Cisp.recursive c init u eval *)

(* this should take node Seq.t, which are 2d coordinates. Now I want you to provide me with a "difference" stream that is the vectors between those points. 
  I would like you to return those vectors as (delta x, delta y) stream, so a stream of float tuples *)
let playEdges nodes_stream =
  nodes_stream |> Cisp.selfChain
  |> Seq.map (fun (Node {x= x1; y= y1; _}, Node {x= x2; y= y2; _}) ->
         (x2 -. x1, y2 -. y1) )

let playNodesAsAnts nodes_sq =
  let play (Node {x; y; _}) =
    let f1 = Cisp.linlin 0.0 1.0 20.0 128.0 x |> Cisp.mtof in
    let imp = Cisp.linlin 0.0 1.0 (-80.0) 20.0 y |> Cisp.mtof in
    let pan = Cisp.linlin 0.0 1.0 (-1.0) 1.0 y in
    SuperAnts.ant_crackle ~freq:f1 ~range:2.0 ~imp ~q:300.0 ~releaseDur:2.0 ~pan
      ()
  in
  let open Cisp in
  functionTrigger play nodes_sq (interval (st 441))

let pathsSignal () =
  let lst_head lst =
    match lst with
    | x :: _ -> Some x
    | [] -> None
  in
  let open Cisp in
  let path_sq = ofRef paths in
  let scale x = (x /. float_of_int num_nodes *. 72.0) +. 64. |> mtof in
  let of_node_list lst =
    lst |> lst_head
    |> fun opt ->
    Option.value opt ~default:[default_node]
    |> fun node_lst ->
    node_lst
    |> List.map (fun i -> i |> get_node_id)
    |> List.to_seq
    |> hold (st 41)
    |> floatify |> fmap scale |> sinosc2
  in
  path_sq |> Seq.map of_node_list |> concat

let paths pheromones () =
  let open Cisp in
  let signal = output pheromones |> Seq.map (fun x -> x *. 40.0) in
  let scale x = (x /. float_of_int num_nodes *. 72.0) +. 64. |> mtof in
  signal |> hold (st 30) |> fmap scale |> sinosc2

let att gain input = input |> Seq.map (fun x -> x *. gain)

(**
let sumOctaves () =
  let open Cisp in
    List.fold_left ( +.~ ) (st 0.0) [
      justThePath () 
    ; justThePath () |> hold (st 2) 
    ; justThePath () |> hold (st 4)
    ; justThePath () |> hold (st 8)
    ; justThePath () |> hold (st 16)
    ; justThePath () |> hold (st 32)] *)

let stereosig holdn pheromones =
  let gainStereo (s1, s2) = (att 1.0 s1, att 1.0 s2) in
  justThePath pheromones holdn |> gainStereo

let bunch s pheromones () =
  let open Cisp in
  let sumPairs (sq1L, sq1R) (sq2L, sq2R) = (sq1L +.~ sq2L, sq1R +.~ sq2R) in
  let stereos =
    [s * 2; s * 3] |> List.to_seq |> fmap (fun n -> stereosig n pheromones)
  in
  let result = Seq.fold_left sumPairs (st 0.0, st 0.0) stereos |> pairToList in
  result

let move_node_by_promille (Node {id; x; y; z; _}) =
  let promille = !current_buffer.brownian in
  let new_x =
    x +. Toolkit.rvfi (-1.0 *. promille) promille |> Toolkit.wrapf 0.0 1.0
  in
  let new_y =
    y +. Toolkit.rvfi (-1.0 *. promille) promille |> Toolkit.wrapf 0.0 1.0
  in
  let new_z =
    z +. Toolkit.rvfi (-1.0 *. promille) promille |> Toolkit.wrapf 0.0 1.0
  in
  Node {id; x= new_x; y= new_y; z= new_z; sync= Modified}

(* Brownian motion thread - runs independently from audio thread *)
let brownian_thread_function () =
  while true do
    (* Sleep for a bit to control update rate *)
    Unix.sleepf 0.01 ;
    (* 100 Hz update rate *)

    (* Only update if brownian is active *)
    if !current_buffer.brownian > 0.0 then
      for
        (* Update all nodes with brownian motion *)
        idx = 0 to num_nodes - 1
      do
        let new_node = move_node_by_promille nodes.(idx) in
        ignore (update_matrix_2_safe idx new_node nodes distance_array)
      done
  done

let push_nodes () =
  let open Cisp in
  countTill (num_nodes - 1)
  |> fmap (fun idx ->
         let new_node = move_node_by_promille nodes.(idx) in
         ignore (update_matrix_2 idx new_node nodes distance_array) ;
         () )
  |> hold (st 100)

let nodes_history recordsArray phers =
  let size = Array.length recordsArray in
  let nds = nodesStream phers () in
  let idx = 0 in
  let open Cisp in
  let writeOneNode idx node =
    if idx < size - 1 then recordsArray.(idx) <- Some node
    else if idx = size then print_endline "📼 recording completed"
    else ()
  in
  let nodeWriter = Seq.map2 writeOneNode count nds in
  pulse (st 10) nodeWriter (st ())

(*start duration envelope index transpose x y*)
let pitch_of_z z = (z *. 24.0) -. 12.0 |> Cisp.mtor

let rolandifier nds =
  let open Wfs in
  let open Cisp in
  let scale x = linlin 0.0 1.0 (-20.0) 20.0 x in
  rolandEvent
  <$> walk 0.0 (st 0.02)
  <*> st 0.1 <*> st Wfs.defaultEnv <*> Seq.map get_node_id nds
  <*> Seq.map (fun n -> n |> get_node_z |> pitch_of_z) nds
  <*> Seq.map (fun n -> n |> get_node_x |> scale) nds
  <*> Seq.map (fun n -> n |> get_node_y |> scale) nds

let csoundGrain = 128.0 /. 44100.0

let createCsound2 filename nodes =
  let open Csound in
  let open Cisp in
  let from_node node number =
    let transpose =
      node |> get_node_z |> linlin 0.0 1.0 (-64.0) 64.0 |> Cisp.mtor
    in
    let start = 0.001 *. (number |> float_of_int) in
    let dur = 128.0 /. 44100.0 *. (1.0 /. transpose) in
    let offset = 44100 * get_node_id node |> intPar in
    let trans = transpose |> floatPar in
    let channel = node |> get_node_id |> intPar in
    let dur2 = dur |> floatPar in
    let attack = 0.00001 |> floatPar in
    let decay = dur |> floatPar in
    let sustain = 0.0 |> floatPar in
    let release = 0.0 |> floatPar in
    fromArgs 1 start dur offset trans channel dur2 attack decay sustain release
  in
  map2 from_node nodes count |> List.of_seq |> Csound.mkScore
  |> render_cscore_to_file filename

type dist_node = DistNode of {d: float; node: node}

let get_delta (DistNode dn) = dn.d

let get_dist_node (DistNode dn) = dn.node

let distnode a b = DistNode {d= distance a b; node= a}

let rec distanced_nodes nodes () =
  let open Seq in
  match nodes () with
  | Cons (first, tail) -> (
    match tail () with
    | Cons (second, tail2) -> Cons (distnode first second, distanced_nodes tail)
    | Nil -> Nil )
  | Nil -> Nil

let createCsound3 filename nodes =
  let dnodes = distanced_nodes nodes in
  let invert_delta x = 1.414 -. x in
  let open Csound in
  let open Cisp in
  let tsteps =
    dnodes
    |> fmap (fun n ->
           get_delta n |> invert_delta
           |> linlin 0.0 1.414 22.0 100.0
           |> mtof
           |> fun x -> 1.0 /. x )
  in
  let starts = walk 0.0 tsteps in
  let from_node start node number =
    let transpose =
      node |> get_node_z |> linlin 0.0 1.0 (-14.0) 14.0 |> Cisp.mtor
    in
    let dur = 128.0 /. 44100.0 *. (1.0 /. transpose) in
    let offset = 44100 * get_node_id node |> intPar in
    let trans = transpose |> floatPar in
    let channel = node |> get_node_id |> intPar in
    let dur2 = dur |> floatPar in
    let attack = 0.00001 |> floatPar in
    let decay = dur |> floatPar in
    let sustain = 0.0 |> floatPar in
    let release = 0.0 |> floatPar in
    (* let dur = 4096.0 /. 44100.0 in
    let offset = 44100 * get_node_id node |> intPar in
    let trans = node |> get_node_z |> linlin 0.0 1.0 0.0 24.0 |> floatPar in
    let channel = node |> get_node_id |> intPar in
    let dur2 = dur |> floatPar in
    let attack = 0.00001 |> floatPar in
    let decay = dur |> floatPar in
    let sustain = 0.0 |> floatPar in
    let release = 0.0 |> floatPar in *)
    fromArgs 1 start dur offset trans channel dur2 attack decay sustain release
  in
  map3 from_node starts nodes count
  |> List.of_seq |> Csound.mkScore
  |> render_cscore_to_file filename

let csoundWithEffect nodes =
  nodes |> createCsound3 "antscore3.sco" ;
  let output_file_name =
    Toolkit.generate_timestamp_filename ~prefix:"output" ~suffix:".wav" ()
  in
  print_endline output_file_name ;
  (* Sys.command ("csound -j8 csound/play_samples.csd -o " ^ output_file_name) *)
  0 |> Printf.printf "skipped running csound, result = %d"
(* print_endline "🔈 csound is finished" *)

let num_channels = 49

(* Define a simple pulse function: e.g., a small Gaussian centered at 0. *)
let finalWrite record_array =
  print_endline "\nwriting Wfs Score\n" ;
  let nodes = record_array |> Array.to_seq |> Seq.filter_map (fun x -> x) in
  nodes |> rolandifier |> List.of_seq |> Wfs.score "roland_ants"
  |> Wfs.score_to_string
  |> Toolkit.write_string_to_file "wfs_score.uscore" ;
  print_endline "✅ wfs score written" ;
  nodes |> csoundWithEffect ;
  print_endline "write just the nodes" ;
  nodes |> Ants.write_node_seq_to_file "nodes.json" ;
  print_endline "✅ nodes json written"

let array_as_infinite_stream arr =
  let length = Array.length arr in
  if length = 0 then failwith "Cannot create stream from empty array"
  else
    (* Create a sequence that repeatedly reads from the array *)
    let rec make_seq position () =
      (* Use modulo to wrap around when we reach the end of the array *)
      let current_pos = position mod length in
      (* Return the current value and a continuation *)
      Seq.Cons (Array.get arr current_pos, make_seq (position + 1))
    in
    make_seq 0

(* Usage example for audio processing *)
let process_audio_stream arr =
  let open Seq in
  array_as_infinite_stream arr |> map (fun x -> x *. 1.0)
(* Apply gain *)

let makeSum arrarr () =
  let open Cisp in
  Array.sub arrarr 0 3
  |> Array.mapi (fun idx arr ->
         process_audio_stream arr |> hold (st (idx + 1)) )
  |> Array.to_list

let rec live_speed () = Seq.Cons (!current_buffer.speed_of_comp, live_speed)

let slower_compute arr =
  let open Cisp in
  pulse live_speed (only_compute arr) (st ())

let one_voice inputArr =
  let clone = duplicateArrArr inputArr in
  let sigL, sigR = stereosig 1 clone in
  [Cisp.syncEffect sigL (slower_compute clone); sigR]

(* Create an infinite sequence from a mutable array that's updated elsewhere *)

(* let graphic () =
  while true do
    Unix.sleepf 0.33 ;
    let svg =
      Graphsvg.generate_svg (Array.map get_coords nodes) pheromones !shortest
    in
    write_to_file
      "/Users/casperschipper/devel/ocaml/cisp/examples/ant_state.svg" svg ;
    ()
  done *)

let encode_nodes nodes =
  let lst = Array.to_list nodes in
  `List
    ( lst
    |> List.map (fun node ->
           node |> get_coords
           |> fun (Coordinate {x; y; z}) -> `List [`Float x; `Float y; `Float z] )
    )

let fromEdge phers (Edge e) =
  let start_coords = get_coords e.start in
  let target_coords = get_coords e.target in
  let start_id = get_node_id e.start in
  let target_id = get_node_id e.target in
  let weight = phers.(start_id).(target_id) in
  DrawableEdge {start= start_coords; target= target_coords; weight}

let simpleEdgesFrom phers =
  phers
  |> Array.mapi (fun i rr ->
         rr
         |> Array.mapi (fun j p -> SimpleEdge {start= i; target= j; weight= p}) )
  |> Toolkit.flatten

let get_x (Coordinate d) = d.x

let get_y (Coordinate d) = d.y

let get_z (Coordinate d) = d.z

let encode_edges edges =
  (* let open Toolkit in *)
  let encode (DrawableEdge {start; target; weight}) =
    `List
      [ `List [`Float (get_x start); `Float (get_y start); `Float (get_z start)]
      ; `List
          [`Float (get_x target); `Float (get_y target); `Float (get_z target)]
      ; `Float weight ]
  in
  `List (edges |> List.map encode)

let encode_simple_edges simple_edges =
  let encode (SimpleEdge {start; target; weight}) =
    `Assoc
      [("start", `Int start); ("target", `Int target); ("weight", `Float weight)]
  in
  `List (simple_edges |> Array.map encode |> Array.to_list)

let edges_from_arrayarray nodes arrr =
  arrr
  |> Array.mapi (fun i arr ->
         arr
         |> Array.mapi (fun j strength ->
                DrawableEdge
                  { start= nodes.(i) |> get_coords
                  ; target= nodes.(j) |> get_coords
                  ; weight= strength } ) )
  |> Toolkit.flatten

let encode_nodes_update new_nodes =
  let encode_node_update (Node {id; x; y; z; _}) =
    `Assoc
      [("node_id", `Int id); ("x", `Float x); ("y", `Float y); ("z", `Float z)]
  in
  `List (new_nodes |> List.map encode_node_update)

let encode_nodes_edges phers nodes =
  let open Yojson in
  let edges = edges_from_arrayarray nodes phers |> Array.to_list in
  let yojson_value =
    `Assoc
      [ ("nodes", encode_nodes nodes)
      ; ("edges", encode_edges edges)
      ; ("numberOfNodes", `Int num_nodes) ]
  in
  try Ok (Safe.to_string yojson_value) with
  | Json_error msg -> Error ("Yojson encoding error: " ^ msg)

let modify_nodes nds =
  (* this will return only the modified nodes, and considet them synced *)
  let number i x = (i, x) in
  nds |> Array.to_list |> List.mapi number
  |> List.filter_map (fun (idx, Node n) ->
         (* only report unsynced nodes *)
         match n.sync with
         | Synced -> None
         | _ ->
             let new_node = Node {n with sync= Synced} in
             nodes.(idx) <- new_node ;
             Some new_node )

let encode_nodes_edges_update phers nodes =
  let open Yojson in
  let edges = simpleEdgesFrom phers in
  let updates = modify_nodes nodes in
  let value =
    `Assoc
      [ ( "update"
        , `Assoc
            [ ("updated_nodes", encode_nodes_update updates)
            ; ("edges", encode_simple_edges edges)
            ; ("controllers", `String (controllers_to_string !current_buffer))
            ] ) ]
  in
  Safe.to_string value

(* Call the function *)

let json_error error_str =
  let open Yojson in
  let json_value = `Assoc [("error", `String error_str)] in
  Safe.to_string json_value

let home = read_file "simplecasper3d.html"

type webCtrl =
  | Drag of {node_id: int; x: float; y: float; z: float}
  | Brownian of float

let handle_websocket_json_update str =
  match Yojson.Safe.from_string str with
  | `Assoc
      [ ( "drag"
        , `Assoc
            [ ("node_id", `Int node_id)
            ; ("x", `Float x)
            ; ("y", `Float y)
            ; ("z", z_opt) ] ) ] ->
      let z =
        match z_opt with
        | `Float f -> f
        | _ -> 0.0
      in
      Ok (Drag {node_id; x; y; z})
  | `Assoc [("brownian", `Assoc [("amount", `Float amount)])] ->
      Ok (Brownian amount)
  | _ -> Error "Unexpected JSON format"

let dream_thread phers nodes () =
  Dream.run ~port:8080 @@ Dream.logger
  @@ Dream.router
       [ Dream.get "/" (fun _ -> Dream.html home)
       ; Dream.get "/websocket" (fun _ ->
             Dream.websocket (fun websocket ->
                 let rec process_messages () =
                   match%lwt Dream.receive websocket with
                   | Some "tick" ->
                       let result = encode_nodes_edges_update phers nodes in
                       let%lwt () =
                         (* let _ = print_endline ("yes: " ^ result) in *)
                         Dream.send ~text_or_binary:`Text websocket result
                       in
                       process_messages () (* Continue looping *)
                   | Some "init" ->
                       let value =
                         `Assoc
                           [ ( "init"
                             , `Assoc
                                 [ ("nodes", encode_nodes nodes)
                                 ; ( "edges"
                                   , encode_simple_edges (simpleEdgesFrom phers)
                                   )
                                 ; ("num_nodes", `Int num_nodes) ] ) ]
                       in
                       let str = Yojson.Safe.to_string value in
                       let%lwt () =
                         Dream.send ~text_or_binary:`Text websocket str
                       in
                       process_messages ()
                   | Some str -> (
                     match handle_websocket_json_update str with
                     (* --- Updated handler usage --- *)
                     | Ok (Drag {node_id; x; y; z}) ->
                         let old = nodes.(node_id) in
                         (* let (Node {z= old_z; _}) = old in *)
                         ignore
                           (update_matrix_2_safe node_id (mkNode node_id x y z)
                              nodes distance_array ) ;
                         process_messages ()
                     | Ok (Brownian amount) ->
                         ignore (update_and_swap ~update:set_brownian amount) ;
                         process_messages ()
                     | Error str ->
                         let _ = print_string ("some error" ^ str) in
                         let%lwt () =
                           Dream.send ~text_or_binary:`Text websocket str
                         in
                         process_messages () )
                   | None -> Dream.close_websocket websocket
                 in
                 process_messages () (* Start processing messages *) ) ) ]

(* oscSender.ml *)

let formatted_time () =
  let open Unix in
  let time = localtime (time ()) in
  Printf.sprintf "%02d-%02d-%02d:%02d"
    (time.tm_mon + 1) (* tm_mon is 0-based, so add 1 *)
    time.tm_mday time.tm_hour time.tm_min

let seqs_to_arrays_in_sync (seqs : float Seq.t list) (n : int) :
    float array array =
  let n_channels = List.length seqs in
  if n_channels = 0 then invalid_arg "No sequences provided" ;
  (* Create one array per channel *)
  let arrays = Array.init n_channels (fun _ -> Array.make n 0.0) in
  (* Convert the list of sequences to a mutable array of thunks (stateful) *)
  let seq_arr = Array.of_list seqs in
  (* For each frame (sample index) *)
  for i = 0 to n - 1 do
    for c = 0 to n_channels - 1 do
      match seq_arr.(c) () with
      | Seq.Nil -> arrays.(c).(i) <- 0.0
      | Seq.Cons (x, next) ->
          arrays.(c).(i) <- x ;
          seq_arr.(c) <- next
    done
  done ;
  arrays

let fold_stereo_pairs_to_flat_list pairs =
  Seq.fold_left (fun acc (sql, sqr) -> sql :: sqr :: acc) [] pairs

let non_realtime2 dur arrarr filename =
  let str =
    Cisp.rangei 0 15
    |> Cisp.fmap (fun _ -> one_voice arrarr)
    |> List.of_seq |> List.concat
  in
  let audio_data = seqs_to_arrays_in_sync str (dur * 48000) in
  Sndfile.write_multichannel_array audio_data 48000 filename Sndfile.WAV_24

let nonrealtime record_length arrarr =
  let sigs =
    [1; 1; 1; 1; 2; 2; 2; 2; 1; 1; 1]
    |> List.map (fun n -> bunch n arrarr ())
    |> List.concat
  in
  let sigs_with_calc =
    (only_compute arrarr |> Seq.map (fun _ -> 0.0)) :: sigs
  in
  let nsamps = !Process.sample_rate *. record_length |> floor |> int_of_float in
  let arrays = seqs_to_arrays_in_sync sigs_with_calc nsamps in
  Sndfile.write_multichannel_array arrays
    (!Process.sample_rate |> floor |> int_of_float)
    "/Users/casperschipper/Music/ants/file2_Smaller_tour.wav" Sndfile.WAV_32

let record_list_of_channels prefix duration_in_secs lst =
  let nsamps =
    !Process.sample_rate *. duration_in_secs |> floor |> int_of_float
  in
  let arrays = seqs_to_arrays_in_sync lst nsamps in
  Sndfile.write_multichannel_array arrays
    (!Process.sample_rate |> floor |> int_of_float)
    ( "/Users/casperschipper/Music/ants/"
    ^ Toolkit.generate_timestamp_filename ~prefix ~suffix:".wav" () )
    Sndfile.WAV_32

(* let distance_position_signal nodes =  *)

let ssp_signal ?(interp = true) node_to_amp nodes =
  let dnodes = nodes |> distanced_nodes in
  let open Cisp in
  let amps = nodes |> fmap (fun x -> (node_to_amp x *. 2.0) -. 1.0) in
  if interp then
    let ts =
      dnodes
      |> fmap (fun x ->
             get_delta x
             |> linpow 0.0 1.4
                  (1.0 /. !Process.sample_rate) (* low speed *)
                  (512. /. !Process.sample_rate) (* top speed *)
                  1.06 )
    in
    render_waveform_minimal (zip amps ts)
  else
    let samps =
      (* not used if interpreted *)
      dnodes
      |> fmap (fun x -> get_delta x |> linlin 0.0 1.4 1.0 20.0 |> int_of_float)
    in
    hold samps amps

let push_nodes_slower = Cisp.pulse live_speed (push_nodes ()) (Cisp.st ())

let octave_from_scalar x =
  x |> Cisp.linlin 0.0 1.0 1.0 16.0 |> floor |> int_of_float

let interval_from_scalar x =
  x |> Cisp.linlin 0.0 1.0 0.0 11.0 |> floor |> int_of_float

let get_frequency node =
  let x = get_node_x node in
  let y = get_node_y node in
  let octave = octave_from_scalar x in
  let interval = interval_from_scalar y in
  let midi = (octave * 12) + interval in
  Cisp.mtof (float_of_int midi)

let frequency_from_nodes nodesStream =
  let open Cisp in
  let freqs = nodesStream |> fmap (fun node -> get_frequency node) in
  freqs

let freq_to_sinewave (samplerate : float) (freq_seq : float Seq.t) : float Seq.t
    =
  let dt = 1.0 /. samplerate in
  let rec next phase seq () =
    match seq () with
    | Seq.Nil -> Seq.Nil
    | Seq.Cons (f, rest) ->
        let new_phase = phase +. (2.0 *. Float.pi *. f *. dt) in
        let value = sin new_phase in
        Seq.Cons (value, next new_phase rest)
  in
  next 0.0 freq_seq

type event = {frequency: float; duration: float; pos: float}

type jv_event = {out: int; dur: float; amp: float; offset: int;transpose:int}
(* { [("out", I out); ("duration", F dur); ("amp", F amp); ("offset", I offset)] |> to_args *)

let test_event f p = {frequency= f; duration= 0.1; pos= p}

let test_jv_event offset transpose =
  {out= offset |> Toolkit.modBy 24; dur= 0.05; amp= 0.1; offset=offset; transpose}

let from_jv_event_to_bundle time {out; dur; amp; offset; transpose} =
  Supercollider.simple_jv ~out ~time ~dur ~amp ~offset ~transpose

let from_event_to_bundle start_time {frequency; duration; pos} =
  Supercollider.simple_tone ~time:start_time ~freq:frequency ~dur:duration ~pos

let send_osc sender time evt =
  let bytes = from_jv_event_to_bundle time evt in
  Supercollider.send_message sender bytes
 
let supercollider_sched nodes_stream nodes_stream2 =
  let scheduler_samps = 4048 in
  let scheduler_sec = Cisp.seconds_from_samples scheduler_samps in
  let event_of_node dnode node2 =
    let tmapping dist = 
      Cisp.linlin 0.0 1.414 (-48.0) 48.0 dist |> Cisp.mtor
    in
    let node = 
      dnode |> get_dist_node 
    in
    ( Float.max 0.0003 (!current_buffer.supercollider_entrydelay *. (tmapping (get_delta dnode)))
    , test_jv_event (get_node_id node |> fun x -> (x + 0) mod 49) (get_node_id node2))
  in
  let event_sq = Infseq.map2 event_of_node (nodes_stream |> distanced_nodes |> Infseq.of_seq) (nodes_stream2 |> Infseq.of_seq)  in
  let sched =
    Clockscheduler.create ~interval:scheduler_sec ~overlap:1.25 ~seq:event_sq
      ~latency:0.3 ~max_events_per_buffer:10000
  in
  let sender = Supercollider.init_sender ~ip:"127.0.0.1" ~port:58000 in
  let create_event time event = send_osc sender time event in
  let sched_sq =
    Cisp.simpleRecursive sched (Clockscheduler.update create_event) ignore
  in
  Cisp.hold (Cisp.st scheduler_samps) sched_sq

let jackMain array () =
  let clock = Cisp.masterClock in
  let array1 = array in
  let array2 = duplicateArrArr array in
  let sq2 = nodesStream array2 () in
  let sq = nodesStream array1 () in
  let final =
    Cisp.effectsSync
      [slower_compute array1; slower_compute array2; clock; supercollider_sched sq sq2]
      (Cisp.st 0.0)
  in
  Jack.playSeqs 0 Process.sample_rate [final]

let monitor_sample_count label n_interval sq =
  let open Cisp in
  zipWith
    (fun idx x ->
      if Toolkit.modBy n_interval idx = 0 then (
        print_endline (label ^ " " ^ string_of_int idx) ;
        x )
      else x )
    count sq

(*
let non_realtime_jackMain title motherArray =
   let voice phers =
    let clone = duplicateArrArr phers in
    let withEffect =        
      nodesStream clone () |> Cisp.effectSync (slower_compute clone)
    in
    only_noisy_paths clone
  in
  let channels =
    Cisp.rangei 0 31
    |> Seq.map (fun i -> voice motherArray)
    |> List.of_seq
  in *)
(* let array1 = duplicateArrArr motherArray in
   let array2 = duplicateArrArr motherArray in
    let applyEffects master =
    List.fold_left Cisp.syncEffect master
      [ slower_compute array1
      ; slower_compute array2
      ; push_nodes_slower
      (* ; othereffect *)
       ]
  in
  let channels =
    [applyEffects (only_noisy_paths array1); only_noisy_paths array2]
  in *)
(* let one_voice ps =
    let clone = duplicateArrArr ps in
    Cisp.effectSync (only_compute ps) (justThePath ps)
  in
  let channels =
    Cisp.rangei 0 31 |> Seq.map (fun i -> one_voice motherArray) |> List.of_seq
  in
  let counted =
    match channels with
    | [] -> []
    | hd :: tail ->
        let counted =
          monitor_sample_count "channel 1, index: "
            (!Process.sample_rate |> int_of_float)
            hd
        in
        counted :: tail
  in
  record_list_of_channels title record_duration counted *)

let floatToMidi flt = flt *. 128.0 |> floor |> int_of_float

let unzip3_seq (s : ('a * 'b * 'c) Seq.t) : 'a Seq.t * 'b Seq.t * 'c Seq.t =
  (* We wrap the source sequence in a ref to share it lazily *)
  let src = ref s in
  let make_proj proj =
    let rec next () =
      match !src () with
      | Seq.Nil -> Seq.Nil
      | Seq.Cons ((a, b, c), tl) ->
          src := tl ;
          Seq.Cons (proj (a, b, c), next)
    in
    next
  in
  ( make_proj (fun (a, _, _) -> a)
  , make_proj (fun (_, b, _) -> b)
  , make_proj (fun (_, _, c) -> c) )

let midiOut phers =
  let open Midi in
  let open Cisp in
  let ctrlStr =
    nodesStream phers ()
    |> Seq.map (fun (Node {id; x; y; z; sync}) ->
           ignore (x, y, z, sync) ;
           (id + 23, floatToMidi x, floatToMidi x) )
  in
  let p, v, d = unzip3_seq ctrlStr in
  let c = p |> Seq.map (fun x -> x mod 15) in
  let f inp =
    let stream = mkNoteClip <$> c <*> p <*> v <*> d in
    let midi = Midi.trigger stream inp |> serialize |> Seq.map toRaw in
    let withEffect = midi |> effectsSync [push_nodes (); only_compute phers] in
    withEffect
  in
  Midi.playMidi f Process.sample_rate

let createCsound filename nodes =
  let open Csound in
  let indexes = nodes |> Seq.map (fun n -> n |> get_node_id) in
  let indexesPar = indexes |> Seq.map Csound.intPar in
  let transposePar =
    indexes
    |> Seq.map (fun i ->
           i |> float_of_int
           |> Cisp.linlin 0.0 49.0 (-12.0) 12.0
           |> Cisp.mtor |> Csound.floatPar )
  in
  let offsets =
    indexes |> Seq.map (fun nid -> (nid + 24) mod 120 * 44100 |> Csound.intPar)
  in
  let open Cisp in
  let score =
    Csound.fromStreams 1
      (walk 0.0 (st csoundGrain))
      (st 0.2)
      [ offsets
      ; transposePar
      ; indexesPar
      ; st (floatPar 0.00001)
      ; st (floatPar 0.2)
      ; st (floatPar 0.0)
      ; st (floatPar 0.0) ]
  in
  render_cscore_to_file filename score

type realmode = RealtimeRecord | RealtimeBundles

type mode = Realtime of realmode | NonRealtime | FromNodes

let program_mode = Realtime RealtimeBundles
(*
Can we generate a supercollider score in parallel to an audio output (where we use the audio output for tuning to a n interesting dynamic.

We need a function that reacts to a ctrl+c  to stop the program and write the score.
The score we might built
*)

let record_array = Array.init 524_288 (fun _ -> None)

let handle_end () =
  print_string "lets close this and write to file" ;
  let _ = Thread.create finalWrite record_array in
  ()

let realtime rmode =
  let phers = mkPheromonesArr in
  (* recording materials *)
  (* let otherEffect = nodes_history record_array phers in *)
  (* let countedEffect = monitor_sample_count "*rec*" 4096 otherEffect in *)
  let _ = Thread.create (dream_thread phers nodes) () in
  let node_stream = nodesStream phers () in
  let _ = Thread.create (jackMain phers) () in
  (*
        let array1 = duplicateArrArr pheromones in *)
  let _ = Thread.create (osc_thread_function handle_end) () in
  (* Start the Brownian motion thread *)
  let _ = Thread.create brownian_thread_function () in
  (* let _ = Thread.create midiOut array1 in *)
  (* let _ =
          print_int
            (Sys.command
              "jack_connect ocaml_midi:ocaml_midi_out system_midi:playback_1" ) ;
          print_int
            (Sys.command
              "jack_connect system_midi:capture_2 ocaml_midi:ocaml_midi_in" ) ;
          ignore (Sys.command "jack_lsp -c -A | grep ocaml")
        in*)
  let port = 8080 in
  let _ = Sys.command (Printf.sprintf "open http://localhost:%d/" port) in
  while true do
    Unix.sleep 1
  done

let () =
  match program_mode with
  | Realtime rmode -> realtime rmode
  | NonRealtime ->
      (* let pheromones = mkPheromonesArr in
      non_realtime2 120 pheromones
        "/Users/casperschipper/Music/ants/slower_convolve.wav" *)
      ()
      (* non_realtime_jackMain "sing_ants_" mkPheromonesArr *)
  | FromNodes ->
      Ants.read_node_seq_from_file "nodes.json" |> createCsound3 "antscore3.sco"
