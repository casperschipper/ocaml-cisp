let generate_random_points ~seed ~count ~max_x ~max_y f =
  (* Set the seed for the random number generator *)
  Random.init seed ;
  (* Create an array of random points *)
  Array.init count (fun idx ->
      let x = Random.float max_x in
      let y = Random.float max_y in
      f idx x y )

let generate_random_points_3d ~seed ~count ~max_x ~max_y ~max_z f =
  (* Set the seed for the random number generator *)
  Random.init seed ;
  (* Create an array of random points *)
  Array.init count (fun idx ->
      let x = Random.float max_x in
      let y = Random.float max_y in
      let z = Random.float max_z in
      f idx x y z )

let generate_grid n f =
  let step = 1.0 /. float_of_int (n - 1) in
  Array.init (n * n) (fun i ->
      let horisteps = i mod n in
      let verticalsteps = i / n in
      let x = float_of_int horisteps *. step in
      let y = float_of_int verticalsteps *. step in
      f i x y )

