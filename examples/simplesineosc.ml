module S = Lo.Server

let buffer_mutex = Mutex.create ()
(* 
type controller =
  { alpha : float
  ; beta : float
  ; deposit : float
  ; evaporation : float } *)
let current_buffer = ref 0.0 
let next_buffer = ref 0.0

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

let handle_osc_message path data =
  match path with
  | "/param" -> 
    begin
      let s = function
      | `Float f | `Double f -> let () = next_buffer := f in ()
      | _ -> () 
    in
    let data = Array.to_list data in
    let _ = List.iter s data in
    ()
  end
  | _ -> Printf.printf "Unhandled OSC path or arguments\n"

let osc_thread_function () =
  let server = S.create 57120 handle_osc_message in
  while true do
    S.recv server;
    swap_buffers ();  (* Swap buffers at a safe point, can be adjusted based on needs *)
  done

let jackMain () =
  let open Cisp in
  let stream = sinosc2 (fromRef current_buffer)  in
  Jack.playSeqs 0 Process.sample_rate [ stream ]

let () = 
   let _ = Thread.create jackMain () in
   let _ = Thread.create osc_thread_function () in
   while(true) do 
    Unix.sleep 20
  done
