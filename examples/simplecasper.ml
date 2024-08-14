module S = Lo.Server

let buffer_mutex = Mutex.create ()

type controllers =
  { alpha : float
  ; beta : float
  ; deposit : float
  ; evaporation : float } 

let initial =
  { alpha = 1.0
  ; beta = 2.0
  ;deposit = 10.0
  ;evaporation = 0.1 }

let set_alpha a ctrl =
  { ctrl with alpha = a }

let set_beta b ctrl =
  { ctrl  with beta = b }

let set_deposit d ctrl = 
  { ctrl  with deposit = d }

let set_evaporation e ctrl = 
  { ctrl  with evaporation = e }


let current_buffer = ref initial
let next_buffer = ref initial

let swap_buffers () = 
  Mutex.lock buffer_mutex;
  let temp = !current_buffer in
  current_buffer := !next_buffer;
  next_buffer := temp;
  Mutex.unlock buffer_mutex


let handle_float_arg datas =
  let data = Array.to_list datas in
  match data with
  | [`Float f] | [`Double f] -> Some f
  | _ -> None

let update_ref_with_option ~ref ~update opt =
  match opt with
  | Some x -> ref := update x !ref 
  | None -> ()

let handle_osc_message path data =
  let update = 
    update_ref_with_option ~ref:next_buffer
  in
  match path with
  | "/alpha" -> data |> handle_float_arg  |> update ~update:set_alpha 
  | "/beta" -> data |> handle_float_arg  |> update ~update:set_beta
  | "/evaporation" -> data |> handle_float_arg  |> update ~update:set_evaporation
  | "/deposit" -> data |> handle_float_arg |> update ~update:set_deposit 
  | _ -> Printf.printf "Unhandled OSC path or arguments\n"

let osc_thread_function () =
  let server = S.create 57666 handle_osc_message in
  while true do
    S.recv server;
    swap_buffers ();  (* Swap buffers at a safe point, can be adjusted based on needs *)
  done




let alpha = 1.0 (* pher paths *)
let beta = 2.99 (* short paths *)
let num_nodes = 19
let evaporation_rate = 0.25
let pheromone_deposition = 40.0
let num_samples = 44100 * 40
let num_ants = 3

type node = Node of { id : int; x : float; y : float }

let default_node = Node { id = 0; x = 0.0; y = 0.0 }

let distance (Node p1) (Node p2) =
  sqrt (((p1.x -. p2.x) ** 2.0) +. ((p1.y -. p2.y) ** 2.0))

(* type dist_arr = float Array.t Array.t *)

module IndexSet = Set.Make (Int)

(* type walk = Walk of { steps : int list; total_dist : float } *)
type edge = Edge of { start : node; target : node; dist : float; inv : float }

let get_dist (Edge e) = e.dist
let get_target (Edge e) = e.target

let get_start (Edge e) = e.start

let get_inverse (Edge e) = e.inv

let mkEdge start target distance =
  Edge { start; target; dist = distance; inv = 1.0 /. distance }

let getDist (Edge e) = e.dist

(* let sort_by_distance arr =
   Array.sort
     (fun a b ->
       let da, db = (getDist a, getDist b) in
       if da == db then 0 else if a < b then 1 else -1)
     arr;
   arr *)

let identity x = x

let generate_distance_matrix points =
  let n = List.length points in
  Array.init n (fun i ->
      Array.init n (fun j ->
          let dist = distance (List.nth points i) (List.nth points j) in
          mkEdge (List.nth points i) (List.nth points j) dist)
      |> Array.to_list |> Array.of_list)

type state =
  | State of {
      current_ant : int;
      current : node;
      targets : node list;
      visited : node list;
      visited_edges : edge list;
      total_dist : float;
      best_dist : float;
    }

(* Function to generate an array of random points *)
let generate_random_points ~seed ~count ~max_x ~max_y =
  (* Set the seed for the random number generator *)
  Random.init seed;
  (* Create an array of random points *)
  Array.init count (fun idx ->
      let x = Random.float max_x in
      let y = Random.float max_y in
      Node { id = idx; x; y })

let logflt label f =
  print_string label;
  print_string " = ";
  print_float f;
  print_newline ()

let logint label i =
  print_string label;
  print_string " = ";
  print_int i;
  print_newline ()

(* Define a function that takes another function `f` and returns a new function that calls `f` once every 1000 times *)
let with_throttled_execution n f = 
  (* Create a reference to keep track of the call count *)
  let call_count = ref 0 in
  fun () -> 
    (* Increment the call count each time the function is called *)
    (* Check if the call count is a multiple of 1000 *)
    if !call_count >= n then
      (* Call the function `f` *)
      let _ = (call_count := 0) in
      f ()
    else
    incr call_count;
      (* Do nothing otherwise *)
      ()

let get_node_id (Node n) = n.id
let get_node_x (Node n) = n.x

let deposit_pher phero_arr tour pheromone =
  List.iter
    (fun (Edge e) ->
      let start_id = get_node_id e.start in
      let target_id = get_node_id e.target in
      let old_value = phero_arr.(start_id).(target_id) in
      phero_arr.(start_id).(target_id) <- pheromone +. old_value)
    tour

(* let pher_from_distance total_dist = 1.0 /. total_dist *)
let get_pheromone p_arr start target = p_arr.(start).(target)

let arr_map_inplace f arr =
  for i = 0 to Array.length arr - 1 do
    arr.(i) <- f arr.(i)
  done

let evaporate p_arr =
  for i  = 0 to Array.length p_arr - 1 do
    for j = 0  to Array.length p_arr.(i) - 1 do
      let old_p_arr = p_arr.(i).(j) in
      p_arr.(i).(j) <- old_p_arr *. (1.0 -. !current_buffer.evaporation)
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
            (let probability = pheromone /. total_pheromone in
            acc -. (probability *. log probability))
          else 
            acc
        else acc)
      0.0 p_arr
  in
  entropy

type 'a debug_state =  { results : 'a list; count : int}

let debug_state = ref { results = [] ; count = 0 }



let debug_sequence pher_arr seq =
  let debug_f x  =
    let state = !debug_state in
    if state.count >= 300 then
      (
      let entropy = calculate_entropy pher_arr in  
      print_string "entropy of row 0 is now: ";
      print_float entropy;
      print_newline ();
      debug_state := {results = entropy :: state.results; count = 0 };
      x)
    else
      let _ = debug_state := {results = state.results ; count = state.count + 1 } in
      x
  in
  seq |> Seq.map debug_f 


type weighted_edge = { edge : edge; weight : float }

let select_next_edge pher_arr edge_arr =
  (* let () = print_endline "*** _+++ start ***" in *)
  if Array.length edge_arr = 1 then Array.get edge_arr 0
  else
    let sum, weighted_edges =
      Array.fold_left
        (fun (acc, lst) edge ->
          let start = get_node_id (get_start edge) in
          let target = get_node_id (get_target edge) in
          let pheromone = pher_arr.(start).(target) in
          let heuristic = get_inverse edge in
          let weight = (pheromone ** !current_buffer.alpha) *. (heuristic ** !current_buffer.beta) in
          (acc +. weight, { edge; weight } :: lst))
        (0.0, []) edge_arr
    in
    let p = Random.float sum in
    let rec pick_if_bigger sumlst acc =
      (* logflt "acc" acc;
         logflt "p" p; *)
      match sumlst with
      | { edge; weight } :: xs ->
          let next_acc = acc +. weight in
          if next_acc >= p then edge
          else
            (* let () = logflt "weight" weight in *)
            pick_if_bigger xs next_acc
      | [] ->
          raise
            (Invalid_argument
               ("unexpected error: you have hit an empty list of edges. p: "
              ^ string_of_float p ^ " acc: " ^ string_of_float acc
              ^ " - sum -  " ^ string_of_float sum))
    in
    pick_if_bigger weighted_edges 0.0

let print_edge (Edge e) =
  print_string "printing edge: ";
  print_string "e.dist - ";
  print_float e.dist;
  print_string " ";
  print_string "e.start .id - ";
  print_int (get_node_id e.start);
  print_string " ";
  print_string "e.target .id - ";
  print_int (get_node_id e.target);
  print_string "\n"

let print_nodes label nodes =
  print_string label;
  List.iter
    (fun x ->
      print_int (get_node_id x);
      print_char ' ')
    nodes

let debug_edges label edges =
  print_endline label;
  List.iter
    (fun (Edge e) ->
      print_int (get_node_id e.target);
      print_char ' ')
    edges;
  print_newline ();
  edges

let remove_node node lst_nodes =
  List.filter (fun n -> get_node_id n != get_node_id node) lst_nodes

let print_int_lst label ilist =
  print_string (label ^ " ");
  List.iter
    (fun i ->
      print_int i;
      print_char ' ')
    ilist;
  print_newline ()

let nodes =
  generate_random_points ~seed:2 ~count:num_nodes ~max_x:100.0 ~max_y:100.0

let distance_array = generate_distance_matrix (Array.to_list nodes)
let pheromones = Array.make_matrix num_nodes num_nodes 1.0


(*
 * complete nodes are all the nodes that are there in the original configuration
 *)
let start_new complete_nodes (State state) =
  let _ =
    deposit_pher pheromones state.visited_edges
      (!current_buffer.deposit /. state.total_dist)
    (* print_string "best distance ";
       print_float state.best_dist;
       print_endline "" *)
  in
  (* reset, all targets become available again *)
  (* let _ =
       print_endline "these are the nodes on reset ";
       print_nodes "state visited (reset): " state.visited;
       print_newline ()
     in *)
  let first_pick = (* choose a random starting point *)
    (* let _ =
         if Array.length reset_targets == 0 then print_endline "OH NO!"
         else print_endline "ok"
       in *)
    Toolkit.choice_arr_opt (complete_nodes |> Array.of_list) |> Option.value ~default:default_node
  in
  let updated_ant =
    if state.current_ant >= num_ants then
      let _ = ignore (evaporate pheromones) in
      0
    else 
      (* let _ = logint "current ant : " state.current_ant in *)
      state.current_ant + 1
  in
  State
    {
      current_ant = updated_ant;
      current = first_pick;
      targets = complete_nodes;
      visited = [];
      total_dist = 0.0;
      visited_edges = [];
      best_dist = min state.best_dist state.total_dist;
    }

let pretty_print_flt_row row =
  let row_string = Array.map (fun flt -> Printf.sprintf "%.2f " flt) row in
  Printf.printf " [ %s ]\n" (String.concat "; " (Array.to_list row_string))

let pretty_print_floats arr = Array.iter pretty_print_flt_row arr


let debug_phers () = 
  print_string "pher :";
  (* print_float (calculate_entropy pheromones.(2)); *)
  pretty_print_floats pheromones;
  print_newline ()

let throttled_pher_func = with_throttled_execution 44100 debug_phers

let pick_next_point pher_arr original_nodes dist_matrix (State state) =
  match state.targets with
  | _ :: _ ->
      (* let _ =
           print_string "current = ";
           print_int (get_node_id state.current);
           print_endline ""
         in *)
      (* let size = Array.length dist_matrix in *)
      (* current should be within bounds *)
      let edges = Array.get dist_matrix (get_node_id state.current) in
      (* let _ = Array.iter print_edge edges in *)
      (* remove current point from targets *)
      let new_targets = remove_node state.current state.targets in
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
        start_new original_nodes (State state)
      else
        let picked_edge = select_next_edge pher_arr filtered_edges in
        let _ = throttled_pher_func () in
        (* let _ = debug_edges "filtered edges" (filtered_edges |> Array.to_list) in *)
        let new_target = picked_edge |> get_target in
        (* let () =
             print_endline "\n visited ";
             print_nodes "old visited state" state.visited;
             print_endline "\n"
           in *)
        State
          {
            current_ant = state.current_ant;
            current = new_target;
            targets = new_targets;
            visited = state.current :: state.visited;
            visited_edges = picked_edge :: state.visited_edges;
            total_dist = state.total_dist +. get_dist picked_edge;
            best_dist = state.best_dist;
          }
  | [] -> start_new original_nodes (State state)

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
  Array.iter pretty_print_row arr



(* let get_x (Node n) = n.x *)

let signal () =
  let nodes_list = nodes |> Array.to_list in
  let init =
    State
      {
        current_ant = 0;
        current = Array.get nodes 0;
        targets = nodes_list;
        visited = [];
        total_dist = 0.0;
        visited_edges = [];
        best_dist = 1000000.0;
      }
  in
  let update = pick_next_point pheromones nodes_list distance_array in
  let eval (State state) = get_node_id state.current in
  Cisp.simpleRecursive init update eval

let line_table =
  let steps = num_nodes in
  let one_step = 1.0 /. float_of_int steps in
  let table = List.init steps (fun i -> float_of_int i *. one_step) in
  table |> Toolkit.shuffle |> Array.of_list

let lookup input = input |> Seq.map (fun idx -> Array.get line_table idx)
let output = signal () |> lookup 

(* let () =
  pretty_print_matrix distance_array;
  print_endline "fish";
  let () = signal () |> Seq.take 100000 |> Seq.iter ignore in
  (* |> List.iter (fun x ->
         print_int x;
         print_string " "); *)
  flush stdout *)

let jackMain () =
   Jack.playSeqs 0 Process.sample_rate
     [
       output
       (* |> Cisp.hold (Cisp.st 40)
          |> Seq.map (fun x -> (x *. 100.0) +. 28.0 |> Cisp.mtof)
          |> Cisp.osc; *)
     ]


let () = 
   let _ = Thread.create jackMain () in
   let _ = Thread.create osc_thread_function () in
   let _ = ignore (Sys.command "jack_connect ocaml:playback_1 ocaml:output_0") in

   while(true) do 
    Unix.sleep 20
  done
