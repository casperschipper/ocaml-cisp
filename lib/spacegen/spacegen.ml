let generate_random_points ~seed ~count ~max_x ~max_y f =
  (* Set the seed for the random number generator *)
  Random.init seed ;
  (* Create an array of random points *)
  Array.init count (fun idx ->
      let x = Random.float max_x in
      let y = Random.float max_y in
      f idx x y )

let generate_random_points_3d ~seed ~count ~max_x ~max_y ~max_z ~f =
  (* Set the seed for the random number generator *)
  Random.init seed ;
  (* Create an array of random points *)
  Array.init count (fun idx ->
      let x = Random.float max_x in
      let y = Random.float max_y in
      let z = Random.float max_z in
      f idx x y z )

let generate_grid n f =
  let step = 0.99 /. float_of_int (n - 1) in
  Array.init (n * n) (fun i ->
      let horisteps = i mod n in
      let verticalsteps = i / n in
      let x = float_of_int horisteps *. step in
      let y = float_of_int verticalsteps *. step in
      f i x y 0.0 )

type vect = {x: float; y: float}

let generate_vectors n amplitude =
  let signed_rand a = (Random.float 2. -. 1.) *. a in
  Array.init n (fun _ -> {x= signed_rand amplitude; y= signed_rand amplitude})

(* Generate [n] (x,y,z) points arranged along [m] parallel lines in 3D.
   All points are normalized into [0,1]^3. *)

(* Generate [n] random points along [m] random lines inside [0,1]^3 *)

let generate_points3d n m =
  Random.self_init ();

  (* Generate M random lines: each as (anchor point, direction) *)
  let lines =
    Array.init m (fun _ ->
      let ax = Random.float 1.0
      and ay = Random.float 1.0
      and az = Random.float 1.0 in
      let dx = (Random.float 2.0 -. 1.0)
      and dy = (Random.float 2.0 -. 1.0)
      and dz = (Random.float 2.0 -. 1.0) in
      ((ax,ay,az), (dx,dy,dz)))
  in

  let points = ref [] in
  while List.length !points < n do
    (* pick a random line *)
    let (ax,ay,az), (dx,dy,dz) = lines.(Random.int m) in

    (* choose a random t in [-0.5,0.5] to stay near the anchor *)
    let t = Random.float 1.0 -. 0.5 in
    let x = ax +. t *. dx
    and y = ay +. t *. dy
    and z = az +. t *. dz in

    (* accept only if inside [0,1]^3 *)
    if x >= 0.0 && x <= 1.0 &&
       y >= 0.0 && y <= 1.0 &&
       z >= 0.0 && z <= 1.0
    then points := (x,y,z) :: !points
  done;
  !points |> Array.of_list



