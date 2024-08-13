let bowie =
  Sndfile.read
    "/Users/casperschipper/Library/CloudStorage/GoogleDrive-casper.schipper@gmail.com/My \
     Drive/GuineaPigs/BowieMono.wav"

let number = 1024
let length = Sndfile.n_frames bowie
let sq = bowie |> fun file -> Sndfile.to_seq file 0
let slice_n = length / number
let sliced = Cisp.group (Cisp.st slice_n) sq |> Cisp.take number
let arr = sliced |> Array.of_seq
let bowie_array = sq |> Seq.take (44100 * 15) |> Array.of_seq

let _ =
  ignore bowie_array;
  arr

type graph = Graph of int Array.t Array.t

let safe_get index arr =
  if index >= 0 && index < Array.length arr then Some (Array.get arr index)
    (* Return the element as an option *)
  else None (* Return None if out of bounds *)

let get_index index (Graph g) = safe_get index g

type state = State of { index : int; graph : graph }

let update () (State { index; graph }) =
  match get_index index graph with
  | None -> State { index = 0; graph }
  | Some i ->
      State
        { index = Toolkit.choice_arr_opt i |> Option.value ~default:0; graph }

let empty =
  Graph
    (Array.init 128 (fun _ ->
         Array.init 1 (fun x -> x + 1 |> Toolkit.modBy 128)))

let init = State { index = 0; graph = empty }
let eval (State { index; _ }) = index
let pi = 3.1415926535

let mixed () =
  Cisp.recursive (Cisp.st ()) init update eval
  |> Seq.map (fun i -> sin (float_of_int i /. 128.0 *. 440.0 *. pi))

let () = Jack.playSeqs 0 Process.sample_rate [ mixed () ]
