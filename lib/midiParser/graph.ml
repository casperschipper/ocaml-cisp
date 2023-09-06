module GraphMap = Map.Make (Int)
module NodeSet = Set.Make (Node)

type t = Graph of NodeSet.t GraphMap.t

let size (Graph g) = GraphMap.fold (fun _ _ x -> x + 1) g 0

let add x y (Graph map) =
  let f opt =
    let ynode = Node.node y in
    match opt with
    | Some nodes -> Some (NodeSet.add ynode nodes)
    | None -> Some (NodeSet.singleton ynode)
  in
  Graph (GraphMap.update x f map)

let passTime (Graph map) =
  let f _ set =
    set
    |> NodeSet.filter_map (fun node ->
           if Node.isOld node then Option.Some (Node.age node) else Option.None )
  in
  GraphMap.map f map

let pick_rand set = set |> NodeSet.to_seq |> Array.of_seq |> Cisp.pickOne

let run (input : int Infseq.t) =
  let update currentValue (graph, prev) =
    (add prev currentValue graph, currentValue)
  in
  let init start = (Graph GraphMap.empty, start) in
  let eval (g, _) = g in
  match input () with
  | Infseq.InfCons (prev, restOfInput) ->
      Infseq.recursive restOfInput (init prev) update eval

let play (graph_seq : t Infseq.t) =
  let update (Graph state) ((currentValue : int), (previousValue : int)) =
    state
    |> GraphMap.find_opt currentValue
    |> Option.map (fun set -> pick_rand set)
    |> fun opt ->
    match opt with
    | Some node -> (previousValue, Node.value node)
    | None -> (previousValue, previousValue)
  in
  let init (Graph state) =
    GraphMap.bindings state
    |> fun bs ->
    match bs with
    | [] -> (0, 0)
    | (k1, _) :: _ -> (k1, 0)
  in
  let start = Infseq.head graph_seq in
  Infseq.recursive graph_seq (init start) update (fun (x, _) -> x)

let print (Graph map) =
  let g node = Node.print node in
  let f key nodes =
    Cisp.debugi "key" key ; NodeSet.iter g nodes ; flush stdout
  in
  Cisp.debugi "map of size:" (size (Graph map)) ;
  GraphMap.iter f map
