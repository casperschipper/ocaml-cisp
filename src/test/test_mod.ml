let xs = List.init (pow 2 16) (fun _ -> 1)

let ys = List.map (fun x -> x + 1) xs
