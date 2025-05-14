module S = Lo.Server

let alpha = 1.0 (*  prefer paths with lots of pheromone *)

let beta = 1.0 (* prefer paths that are shorter *)

let num_nodes = 49

let n_side = num_nodes |> float_of_int |> sqrt |> int_of_float

let evaporation = 0.2

let exploration_bias = 0.001

let deposit = 1.0

let num_ants = 10

let num_samples = 44100 * 40

let max_tour = 33

let brownian = 0.0

(* to update parameters over OSC and/or websocket while the audio thread is running *)
let buffer_mutex = Mutex.create ()

let shortest = ref 0.0

type sync = Modified | Synced | Pristine

type node = Node of {id: int; x: float; y: float; sync: sync}

let update_position x y (Node old) = Node {id= old.id; x; y; sync= Modified}

type edge = Edge of {start: node; target: node; dist: float; inv: float}

type drawable_edge =
  | DrawableEdge of {start: float * float; target: float * float; weight: float}

type simple_edge = SimpleEdge of {start: int; target: int; weight: float}

let paths : node list list ref = ref []

let mkNode id x y = Node {id; x; y; sync= Pristine}

(* Function to generate an array of random points *)

(* |> Array.mapi (fun idx (x, y) -> mkNode idx x y) *)
(* [|(0.25, 0.25); (0.75, 0.25); (0.75, 0.75); (0.25, 0.75)|] *)

let nodes =
  let seed = 123 (*121*) |> Cisp.debugi "my random seed" in
  (* let seed = Random.int 12000 |> Cisp.debugi "myseed" in *)
  (* generate_grid n_side *)
  Spacegen.generate_random_points ~seed ~count:num_nodes ~max_x:1.0 ~max_y:1.0
    mkNode

let distance (Node p1) (Node p2) =
  if p1.id = p2.id then 0.0
  else sqrt (((p1.x -. p2.x) ** 2.0) +. ((p1.y -. p2.y) ** 2.0))

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

(*
let update_matrix point_index new_point points (Distance dist) =
  if point_index > 0 && point_index < Array.length points then (
    points.(point_index) <- new_point ;
    let old_line = dist.(point_index) in
    dist.(point_index) <-
      Array.init num_nodes (fun idx ->
          mkEdge new_point points.(idx) (distance new_point points.(idx)) ) ;
    Array.iteri
      (fun idx line ->
        line.(point_index) <-
          mkEdge nodes.(idx) nodes.(point_index)
            (distance points.(idx) points.(point_index)) )
      dist
    (* ; pretty_print_matrix dist *) )
  else ()
    *)

let update_matrix_2 point_index new_point points (Distance dist) =
  if point_index >= 0 && point_index < Array.length points then (
    points.(point_index) <- new_point ;
    for i = 0 to num_nodes - 1 do
      let start1, start2 = (point_index, i) in
      let target1, target2 = (i, point_index) in
      dist.(start1).(start2) <-
        mkEdge points.(start1) points.(start2)
          (distance points.(start1) points.(start2)) ;
      dist.(target1).(target2) <-
        mkEdge points.(target1) points.(target2)
          (distance points.(target1) points.(target2))
    done ;
    Distance dist (* ; pretty_print_matrix dist *) )
  else Distance dist

(*
  0    1   2
0     0,1
1 1,0 1,1 1,2
2     2,1

*)

(* let debug_distance_array arr =
   arr |> Array.iter (fun line ->
     line |> Array.iter (fun (Edge e) -> print_float e.dist )
     ; print_newline ()
   ) *)

let distance_array = generate_distance_matrix nodes

let mkPheromonesArr = Array.make_matrix num_nodes num_nodes 0.0

let duplicateArrArr arrarr = 
  Array.map Array.copy arrarr

type controllers =
  { alpha: float
  ; beta: float
  ; deposit: float
  ; evaporation: float
  ; num_ants: int
  ; exploration_bias: float
  ; max_tour: int
  ; brownian: float }

let controllers_to_string ctr =
  Printf.sprintf
    "Controllers {\n\
    \  alpha: %.2f\n\
    \  beta: %.2f\n\
    \  deposit: %.2f\n\
    \  evaporation: %.2f\n\
    \  num_ants: %d\n\
    \  max_tour: %d\n\
     }"
    ctr.alpha ctr.beta ctr.deposit ctr.evaporation ctr.num_ants ctr.max_tour

let initial =
  { alpha
  ; beta
  ; deposit
  ; evaporation
  ; num_ants
  ; exploration_bias
  ; max_tour
  ; brownian }

let set_alpha a ctrl = {ctrl with alpha= a}

let set_beta b ctrl = {ctrl with beta= b}

let set_deposit d ctrl = {ctrl with deposit= d}

let set_evaporation e ctrl = {ctrl with evaporation= e}

let set_ants a ctrl = {ctrl with num_ants= a}

let set_brownian a ctrl = {ctrl with brownian= a}

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
      (* let _ = Cisp.debugf "float: " f in *)
      Some f
  | _ -> None

type indexed_point = IndexedPoint of {idx: int; x: float; y: float}

let handle_int_float_float datas =
  let data = Array.to_list datas in
  match data with
  | [`Int32 idx; `Float x; `Float y] -> Some (IndexedPoint {idx; x; y})
  | _ -> None

let handle_int_arg datas =
  let data = Array.to_list datas in
  match data with
  | [`Int32 i]
   |[`Int64 i] ->
      Some i
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

let handle_osc_message path data =
  let update ~update opt =
    opt |> Option.iter (fun x -> update_and_swap ~update x)
  in
  let update_points opt_ipoint =
    match opt_ipoint with
    | Some (IndexedPoint {idx; x; y}) ->
        if idx < num_nodes && idx >= 0 then
          ignore (update_matrix_2 idx (mkNode idx x y) nodes distance_array)
        else ()
    | None -> ()
  in
  match path with
  | "/alpha" -> data |> handle_float_arg |> update ~update:set_alpha
  | "/beta" -> data |> handle_float_arg |> update ~update:set_beta
  | "/evaporation" -> data |> handle_float_arg |> update ~update:set_evaporation
  | "/deposit" -> data |> handle_float_arg |> update ~update:set_deposit
  | "/num_ants" ->
      data |> handle_int_arg
      |> Option.iter (fun f -> update_and_swap ~update:set_ants f)
  | "/point" -> data |> handle_int_float_float |> update_points
  | "/exploration_bias" ->
      data |> handle_float_arg |> update ~update:set_exploration_bias
  | "/max_tour" -> data |> handle_int_arg |> update ~update:set_max_tour
  | _ -> Printf.printf "Unhandled OSC path or arguments\n"

let osc_thread_function () =
  let server = S.create 57666 handle_osc_message in
  while true do
    S.recv server ;
    swap_buffers ()
    (* Swap buffers at a safe point, can be adjusted based on needs *)
  done

let print_sync sync =
  match sync with
  | Pristine -> "pristine"
  | Modified -> "modified"
  | Synced -> "synced"

let print_node = function
  | Node {id; x; y; sync} ->
      let sync = print_sync sync in
      Printf.printf "Node {\n" ;
      Printf.printf "  id: %d\n" id ;
      Printf.printf "  x: %.2f\n" x ;
      Printf.printf "  y: %.2f\n" y ;
      Printf.printf "  sync: %s" sync ;
      Printf.printf "}\n"

let get_node_id (Node n) = n.id

let get_node_x (Node n) = n.x

let default_node = Node {id= 0; x= 0.0; y= 0.0; sync= Synced}

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

let get_coords (Node {x; y; _}) = (x, y)

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

(* this creates a stereo signal, navigating through the pheromones only *)
(* the repeats allows for different update rates , where 1 is sr *)
let justThePath pheromones repeats =
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
    let ix = if t > 10000 then random_index () else model in
    (* TODO think about the 10000 as a parameter *)
    match next_pher with
    | Some i -> (i, ix)
    | None -> (random_index (), 0)
  in
  let eval (inp, _) = nodes.(inp) |> fun (Node node) -> (node.x, node.y) in
  let init = (random_index (), 0) in
  Cisp.recursive c init u eval |> Cisp.hold (st repeats) |> Seq.unzip

let nodesStream pheromones () =
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
    let ix = if t > 10000 then random_index () else model in
    (* TODO think about the 10000 as a parameter *)
    match next_pher with
    | Some i -> (i, ix)
    | None -> (random_index (), 0)
  in
  let eval (inp, _) = nodes.(inp) in
  let init = (random_index (), 0) in
  Cisp.recursive c init u eval

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

let bunch s pheromones () =
  let open Cisp in
  let gainStereo (s1, s2) = (att 0.01 s1, att 0.01 s2) in
  let sumPairs (sq1L, sq1R) (sq2L, sq2R) = (sq1L +.~ sq2L, sq1R +.~ sq2R) in
  let stereos =
    [s] |> List.to_seq
    |> fmap (fun x -> justThePath pheromones x |> gainStereo)
  in
  let result = Seq.fold_left sumPairs (st 0.0, st 0.0) stereos |> pairToList in
  result

let move_node_by_promille (Node {id; x; y; _}) =
  let promille = !current_buffer.brownian in
  let new_x =
    x +. Toolkit.rvfi (-1.0 *. promille) promille |> Toolkit.wrapf 0.0 1.0
  in
  let new_y =
    y +. Toolkit.rvfi (-1.0 *. promille) promille |> Toolkit.wrapf 0.0 1.0
  in
  Node {id; x= new_x; y= new_y; sync= Modified}

let push_nodes () =
  let open Cisp in
  countTill (num_nodes - 1)
  |> fmap (fun idx ->
         let new_node = move_node_by_promille nodes.(idx) in
         ignore (update_matrix_2 idx new_node nodes distance_array) ;
         () )
  |> hold (st 100)

let jackMain array1 array2 () =
  let applyEffects master =
    List.fold_left Cisp.syncEffect master [push_nodes ();only_compute array1 ;only_compute array2 ]
  in
  Jack.playSeqs 0 Process.sample_rate
    ( bunch 1 array1 () @ bunch 1 array2 ()
    @ [applyEffects (Cisp.st 0.0)] )

let write_to_file filename str =
  let oc = open_out filename in
  output_string oc str ; close_out oc

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
           node |> get_coords |> fun (x, y) -> `List [`Float x; `Float y] ) )

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

let encode_edges edges =
  let open Toolkit in
  let encode (DrawableEdge {start; target; weight}) =
    `List
      [ `List [`Float (fst start); `Float (sec start)]
      ; `List [`Float (fst target); `Float (sec target)]
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
  let encode_node_update node =
    let x, y = get_coords node in
    `Assoc
      [("node_id", `Int (get_node_id node)); ("x", `Float x); ("y", `Float y)]
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

let home = read_file "simplecasper.html"

type webCtrl = Drag of {node_id: int; x: float; y: float} | Brownian of float

let handle_websocket_json_update str =
  match Yojson.Safe.from_string str with
  | `Assoc
      [ ( "drag"
        , `Assoc [("node_id", `Int node_id); ("x", `Float x); ("y", `Float y)]
        ) ] ->
      Ok (Drag {node_id; x; y})
  | `Assoc [("brownian", `Assoc [("amount", `Float amount)])] ->
      Ok (Brownian amount)
  | _ ->
      Error
        ( "json has unexpected format:\n " ^ str
        ^ "\nI expected something like\n { node_id : 42, x : 0.3, y : 0.5 }" )

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
                     | Ok (Drag {node_id; x; y}) ->
                         ignore
                           (update_matrix_2 node_id (mkNode node_id x y) nodes
                              distance_array ) ;
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

let () =
  let pheromones = mkPheromonesArr in
  let array1, array2 = (duplicateArrArr pheromones, duplicateArrArr pheromones) in
  let _ = pretty_print_distance distance_array in
  let _ = Thread.create (jackMain array1 array2) () in
  let _ = Thread.create osc_thread_function () in
  (* let _ = Thread.create graphic () in *)
  let _ = Thread.create (dream_thread array1 nodes) () in
  (* let _ = ignore (Sys.command "jack_connect ocaml:playback_1 ocaml:output_0") in *)
  while true do
    Unix.sleep 20
  done
