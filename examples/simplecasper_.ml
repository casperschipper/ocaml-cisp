type node = Node of { id : int ; x : float; y : float }
type graph = Graph of node list

let distance (Node p1) (Node p2) =
  sqrt (((p1.x -. p2.x) ** 2.0) +. ((p1.y -. p2.y) ** 2.0))

type dist_arr =
  float Array.t Array.t

module IndexSet = Set.Make(Int)

type walk = 
  Walk of IndexSet.t

type edge =
  Edge of { dist : float; inv : float; target : int }

let mkEdge d t =
  Edge { dist = d ; inv = 1.0 /. d ; target = t}

let getDist (Edge e) =
  e.dist

let sort_by_distance arr = 
  Array.sort (fun a b -> let da, db = (getDist a,getDist b) in if da == db then 0 else if a < b then 1 else -1) arr; arr

    
let identity x = x

let generate_distance_matrix points =
  let n = List.length points in
  Array.init n (fun i ->
    Array.init n (fun j ->
      if i = j then None else
        let dist = distance (List.nth points i) (List.nth points j) in
       Some (mkEdge dist j)
    ) 
    |> Array.to_list 
    |> List.filter_map identity |> Array.of_list |> sort_by_distance
  )

type state = State of { current : int 
; targets : node list ; visited : node list ; total_dist : float }

(* Function to generate an array of random points *)
let generate_random_points ~seed ~count ~max_x ~max_y =
  (* Set the seed for the random number generator *)
  Random.init seed;
  (* Create an array of random points *)
  Array.init count (fun idx ->
    let x = Random.float max_x in
    let y = Random.float max_y in
    Node { id = idx; x = x; y = y }
  )

let select_with_preference_for_short dist_array =
  let sum = Array.fold_left (fun acc (Edge e) -> acc +. e.inv) 0.0 dist_array in
  let p = Random.float sum in
  (* 0.1 0.2 0.3 0.6 0.9 1.1 *)
  let rec foo lst acc in
  match lst with
  Edge edge :: xs -> if acc > p then return 
  [] -> None
  


let pick_next_point distances current targets visited =
  let size = Array.length distances in
  (* current should be within bounds *)
  let distances = (Array.get distances current) |> Array.to_list in
  (* remove current point, as we are now visiting there *)
  let new_targets = List.filter (fun (Node node) -> node.id != current) targets in



let traverse (Graph nodes) =
    let go (State { current; targets ; visited; total_dist }) =
      
      
     


let () =
  Jack.playSeqs 0 Process.sample_rate
    [ Cisp.effectSync Cisp.masterClock (noisysine ()); noisysine () ];
  Unix.sleep 1;
  ignore (Sys.command "jack_connect ocaml:playback_1 ocaml:output_0")
