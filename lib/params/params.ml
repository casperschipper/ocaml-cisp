(** Parameter generation and updating module

    This module provides an applicative interface for generating
    parameters from infinite sequences and sending them via HTTP POST.
*)

open Lwt.Infix

(** A parameter builder combining multiple sequences *)
type 'a t = { build : unit -> 'a }

(** Create a parameter builder from a sequence *)
let of_seq seq =
  let state = ref seq in
  {
    build =
      (fun () ->
        match Seq.uncons !state with
        | Some (value, rest) ->
            state := rest ;
            value
        | None -> failwith "Sequence exhausted" )
  }

(** Constant parameter (never changes) *)
let const value = { build = (fun () -> value) }

(** Map over a parameter builder *)
let map f t = { build = (fun () -> f (t.build ())) }

(** Applicative apply *)
let apply tf tx = { build = (fun () -> (tf.build ()) (tx.build ())) }

(** Applicative operator *)
let ( <*> ) = apply

(** Applicative map operator *)
let ( <$> ) f t = map f t

(** Lift a binary function *)
let lift2 f ta tb = f <$> ta <*> tb

(** Lift a ternary function *)
let lift3 f ta tb tc = f <$> ta <*> tb <*> tc

(** Lift a 4-ary function *)
let lift4 f ta tb tc td = f <$> ta <*> tb <*> tc <*> td

(** Lift a 5-ary function *)
let lift5 f ta tb tc td te = f <$> ta <*> tb <*> tc <*> td <*> te

(** Build the next parameter value *)
let build t = t.build ()

(** Parameter update configuration *)
type config = {
  url : string;  (** URL to POST parameters to *)
  interval : float;  (** Interval in seconds between updates *)
}

(** Default configuration *)
let default_config = {
  url = "http://localhost:8765/update_params";
  interval = 1.0;
}

(** Send parameters via HTTP POST *)
let send_params url json_string =
  let open Cohttp in
  let open Cohttp_lwt_unix in
  let headers = Header.init_with "Content-Type" "application/json" in
  let body = Cohttp_lwt.Body.of_string json_string in
  Lwt.catch
    (fun () ->
      Client.post ~headers ~body (Uri.of_string url) >>= fun (resp, body) ->
      let code = resp |> Response.status |> Code.code_of_status in
      Cohttp_lwt.Body.drain_body body >>= fun () ->
      if code >= 200 && code < 300 then
        Lwt.return_ok ()
      else
        Lwt.return_error (Printf.sprintf "HTTP error: %d" code)
    )
    (fun exn ->
      Lwt.return_error (Printf.sprintf "Network error: %s" (Printexc.to_string exn))
    )

(** Run parameter generator loop

    This continuously generates parameters using the builder and sends
    them via HTTP POST at the specified interval.

    @param config Configuration for URL and interval
    @param builder Parameter builder
    @param to_json Function to convert parameter value to JSON string
*)
let run_loop config builder to_json =
  let rec loop () =
    let params = build builder in
    let json_str = to_json params in
    send_params config.url json_str >>= fun result ->
    (match result with
    | Ok () -> Lwt_io.printf "Parameters sent successfully\n"
    | Error err -> Lwt_io.printf "Failed to send parameters: %s\n" err
    ) >>= fun () ->
    Lwt_unix.sleep config.interval >>= fun () ->
    loop ()
  in
  loop ()

(** Example: Create oscillating parameter from sine wave *)
let oscillate ~min ~max ~period =
  let freq = 2.0 *. Float.pi /. period in
  let seq = Seq.ints 0 |> Seq.map (fun i ->
    let t = float_of_int i in
    let normalized = (sin (freq *. t) +. 1.0) /. 2.0 in
    min +. (normalized *. (max -. min))
  ) in
  of_seq seq

(** Example: Create linearly interpolating parameter *)
let lerp ~start ~stop ~steps =
  let seq = Seq.init steps (fun i ->
    let t = float_of_int i /. float_of_int (steps - 1) in
    start +. (t *. (stop -. start))
  ) |> Seq.cycle in
  of_seq seq

(** Example: Create random walk parameter *)
let random_walk ~start ~step_size =
  let rec walk value () =
    let delta = (Random.float (2.0 *. step_size)) -. step_size in
    let next = value +. delta in
    Seq.Cons (next, walk next)
  in
  of_seq (walk start)

(** Example: Create stepped parameter that cycles through values *)
let cycle values =
  of_seq (List.to_seq values |> Seq.cycle)
