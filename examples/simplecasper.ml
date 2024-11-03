module S = Lo.Server

let alpha = 1.0 (*  prefer paths with lots of pheromone *)

let beta = 0.5 (* prefer paths that are shorter *)

let num_nodes = 40

let evaporation = 0.2

let deposit = 100.0

let num_ants = 3

let num_samples = 44100 * 40

let buffer_mutex = Mutex.create ()

type node = Node of {id: int; x: float; y: float}

type edge = Edge of {start: node; target: node; dist: float; inv: float}

let mkNode id x y = Node {id; x; y}

(* Function to generate an array of random points *)
let generate_random_points ~seed ~count ~max_x ~max_y =
  (* Set the seed for the random number generator *)
  Random.init seed ;
  (* Create an array of random points *)
  Array.init count (fun idx ->
      let x = Random.float max_x in
      let y = Random.float max_y in
      mkNode idx x y )

let nodes =
  let seed = Random.int 12000 |> Cisp.debugi "myseed" in
  generate_random_points ~seed ~count:num_nodes ~max_x:100.0 ~max_y:100.0

let distance (Node p1) (Node p2) =
  if p1.id = p2.id then 
    0.0
  else 
    sqrt (((p1.x -. p2.x) ** 2.0) +. ((p1.y -. p2.y) ** 2.0))

let get_inverse (Edge e) = e.inv

let mkEdge start target distance =
  Edge {start; target; dist= distance; inv= 1.0 /. distance}

type distance1d = Distance of edge Array.t Array.t

let debugi label i = print_string label ; print_int i ; print_newline ()

let generate_distance_matrix points =
  let n = Array.length points in
  Distance
    (Array.init n (fun i ->
         Array.init n (fun j ->
             let dist = distance (points.(i)) (points.(j)) in
             mkEdge (points.(i)) (points.(j)) dist)
      ) )

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
  Array.iter pretty_print_row arr

(* Function to calculate the index in the condensed matrix *)

let update_matrix point_index new_point points (Distance dist) =
   points.(point_index) <- new_point;
   let old_line = dist.(point_index) in
   dist.(point_index) <- Array.init num_nodes (fun idx -> mkEdge new_point points.(idx) (distance new_point points.(idx) ));
   Array.iteri (fun idx line -> line.(point_index) <- mkEdge nodes.(idx) nodes.(point_index) (distance points.(idx) points.(point_index))) dist
   ; pretty_print_matrix dist


(*
    0 1 2 3
0     
1
2
3

*)

(* let debug_distance_array arr = 
  arr |> Array.iter (fun line -> 
    line |> Array.iter (fun (Edge e) -> print_float e.dist )
    ; print_newline ()
  ) *)

  

let distance_array = generate_distance_matrix nodes

let pheromones = Array.make_matrix num_nodes num_nodes 1.0

type controllers =
  {alpha: float; beta: float; deposit: float; evaporation: float; num_ants: int}

let initial = {alpha; beta; deposit; evaporation; num_ants}

let set_alpha a ctrl = {ctrl with alpha= a}

let set_beta b ctrl = {ctrl with beta= b}

let set_deposit d ctrl = {ctrl with deposit= d}

let set_evaporation e ctrl = {ctrl with evaporation= e}

let set_ants a ctrl = {ctrl with num_ants= a}

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

let update_ref_with_option ~ref ~update opt =
  match opt with
  | Some x -> ref := update x !ref
  | None -> ()

(* let update_distance_matrix nodes old_distances index new_point =
   let copy = Array.copy old_distances in
   nodes.(index) <- new_point;
   Seq.ints 0 |> Seq.take num_nodes |> Seq.map (fun idx ->
     let new_dist = distance new_point nodes.(idx) in
     Toolkit.update_at_index old_distances idx (fun _ -> new_dist)) *)

let handle_osc_message path data =
  let update = update_ref_with_option ~ref:next_buffer in
  let update_points opt_ipoint =
    match opt_ipoint with
    | Some (IndexedPoint {idx; x; y}) ->
        update_matrix idx (mkNode idx x y) nodes distance_array
    | None -> ()
  in
  match path with
  | "/alpha" -> data |> handle_float_arg |> update ~update:set_alpha
  | "/beta" -> data |> handle_float_arg |> update ~update:set_beta
  | "/evaporation" -> data |> handle_float_arg |> update ~update:set_evaporation
  | "/deposit" -> data |> handle_float_arg |> update ~update:set_deposit
  | "/num_ants" ->
      data |> handle_int_arg
      |> update_ref_with_option ~ref:next_buffer ~update:set_ants
  | "/point" -> data |> handle_int_float_float |> update_points
  | _ -> Printf.printf "Unhandled OSC path or arguments\n"

let osc_thread_function () =
  let server = S.create 57666 handle_osc_message in
  while true do
    S.recv server ;
    swap_buffers ()
    (* Swap buffers at a safe point, can be adjusted based on needs *)
  done

let print_node = function
  | Node {id; x; y} ->
      Printf.printf "Node {\n" ;
      Printf.printf "  id: %d\n" id ;
      Printf.printf "  x: %.2f\n" x ;
      Printf.printf "  y: %.2f\n" y ;
      Printf.printf "}\n"

let get_node_id (Node n) = n.id

let get_node_x (Node n) = n.x

let default_node = Node {id= 0; x= 0.0; y= 0.0}

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
      ; best_dist: float }

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

let print_state = function
  | State
      { current_ant
      ; current
      ; targets
      ; visited
      ; visited_edges
      ; total_dist
      ; best_dist } ->
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

let evaporate p_arr =
  for i = 0 to Array.length p_arr - 1 do
    for j = 0 to Array.length p_arr.(i) - 1 do
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

(* let select_next_edge pher_arr edge_arr =
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
           let weight =
             (pheromone ** !current_buffer.alpha)
             *. (heuristic ** !current_buffer.beta)
           in
           (acc +. weight, { edge; weight } :: lst))
         (0.0, []) edge_arr
     in
     if sum = 0.0 then Array.get edge_arr (Random.int (Array.length edge_arr))
     else
       let p = Random.float sum in
       let rec pick_if_bigger sumlst acc =
         (* logflt "acc" acc;
            logflt "p" p; *)
         match sumlst with
         | { edge; weight } :: xs ->
             let next_acc = acc +. weight in
             if p <= next_acc then edge
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
       pick_if_bigger weighted_edges 0.0 *)

type 'a weighted = {w: float; item: 'a}

let pick_weighted lst =
  let total_weight = List.fold_left (fun acc x -> x.w +. acc) 0.0 lst in
  let p = Random.float 1.0 in
  (* let _ = print_float p; print_endline " p" in
     let _ = print_float total_weight ; print_endline " total" in *)
  let rec aux current_sum remaining =
    match remaining with
    | [] -> None
    | [x] -> Some x.item
    | x :: xs ->
        let next = (x.w /. total_weight) +. current_sum in
        (* let _ = print_float next ; print_endline " next" in *)
        if p >= next then aux next xs else Some x.item
  in
  aux 0.0 lst

(* let test_weighted () =
    let test = [1.0;4.0;2.0] in
    let foo = test |> List.mapi (fun i x -> {w = x;item = i}) in
    let bar = Seq.init 100 (fun _-> pick_weighted foo) |> List.of_seq in
    let result = List.filter_map (fun x -> x) bar in
    result *)

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
  let first_pick =
    (* choose a random starting point *)
    (* let _ =
         if Array.length reset_targets == 0 then print_endline "OH NO!"
         else print_endline "ok"
       in *)
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
  State
    { current_ant= updated_ant
    ; current= first_pick
    ; targets= complete_nodes
    ; visited= []
    ; total_dist= 0.0
    ; visited_edges= []
    ; best_dist= min state.best_dist state.total_dist }

let pretty_print_flt_row row =
  let row_string = Array.map (fun flt -> Printf.sprintf "%.2f " flt) row in
  Printf.printf " [ %s ]\n" (String.concat "; " (Array.to_list row_string))

let pretty_print_floats arr = Array.iter pretty_print_flt_row arr

let debug_phers () =
  print_string "pher :" ;
  pretty_print_floats pheromones ;
  print_float (calculate_entropy pheromones.(2)) ;
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
      let edges = get_line (get_node_id state.current) dist_matrix in
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
        let picked_edge = select_next_edge_new pher_arr filtered_edges in
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
          ; best_dist= state.best_dist }
  | [] -> start_new original_nodes (State state)



(* let get_x (Node n) = n.x *)

let signal () =
  let nodes_list = nodes |> Array.to_list in
  let init =
    State
      { current_ant= 0
      ; current= Array.get nodes 0
      ; targets= nodes_list
      ; visited= []
      ; total_dist= 0.0
      ; visited_edges= []
      ; best_dist= 1000000.0 }
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

let otherSignal () =
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
  Cisp.recursive c 0 u (fun index -> line_table.(index))

let jackMain () =
  Jack.playSeqs 0 Process.sample_rate [otherSignal (); otherSignal (); output]

let write_to_file filename str =
  let oc = open_out filename in
  output_string oc str ; close_out oc

let graphic () =
  while true do
    Unix.sleepf 0.33 ;
    let svg = Graphsvg.generate_svg (Array.map get_coords nodes) pheromones in
    write_to_file
      "/Users/casperschipper/devel/ocaml/ocaml-cisp/examples/ant_state.svg" svg ;
    ()
  done

let () =
  let _ = Thread.create jackMain () in
  let _ = Thread.create osc_thread_function () in
  let _ = Thread.create graphic () in
  (* let _ = ignore (Sys.command "jack_connect ocaml:playback_1 ocaml:output_0") in *)
  while true do
    Unix.sleep 20
  done
