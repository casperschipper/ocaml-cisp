(** Parameter generation and updating module

    This module provides an applicative interface for generating
    parameters from infinite sequences and sending them via HTTP POST.

    Example usage:
    {[
      open Params

      (* Define parameter builder using applicative style *)
      let param_builder =
        lift5 (fun speed force sep align coh ->
          `Assoc [
            ("max_speed", `Float speed);
            ("max_force", `Float force);
            ("separation_radius", `Float sep);
            ("alignment_radius", `Float align);
            ("cohesion_radius", `Float coh);
          ])
          (oscillate ~min:20.0 ~max:100.0 ~period:50.0)
          (const 150.0)
          (oscillate ~min:3.0 ~max:15.0 ~period:30.0)
          (const 30.0)
          (const 5.0)

      (* Convert to JSON and run *)
      let () =
        Lwt_main.run (run_loop default_config param_builder Yojson.Basic.to_string)
    ]}
*)

(** A parameter builder that generates values from sequences *)
type 'a t

(** {1 Creating parameter builders} *)

val of_seq : 'a Seq.t -> 'a t
(** Create a parameter builder from an infinite sequence *)

val const : 'a -> 'a t
(** Create a constant parameter that never changes *)

(** {1 Applicative operations} *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** Map a function over a parameter builder *)

val apply : ('a -> 'b) t -> 'a t -> 'b t
(** Applicative apply *)

val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
(** Applicative apply operator *)

val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t
(** Applicative map operator (fmap) *)

val lift2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
(** Lift a binary function into the applicative *)

val lift3 : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t
(** Lift a ternary function *)

val lift4 : ('a -> 'b -> 'c -> 'd -> 'e) -> 'a t -> 'b t -> 'c t -> 'd t -> 'e t
(** Lift a 4-ary function *)

val lift5 : ('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> 'a t -> 'b t -> 'c t -> 'd t -> 'e t -> 'f t
(** Lift a 5-ary function *)

val build : 'a t -> 'a
(** Generate the next value from the parameter builder *)

(** {1 Parameter update configuration} *)

type config = {
  url : string;  (** URL to POST parameters to *)
  interval : float;  (** Interval in seconds between updates *)
}

val default_config : config
(** Default configuration: url=http://localhost:8765/update_params, interval=1.0s *)

(** {1 Running the parameter loop} *)

val send_params : string -> string -> (unit, string) result Lwt.t
(** Send parameter JSON string via HTTP POST to the given URL *)

val run_loop : config -> 'a t -> ('a -> string) -> unit Lwt.t
(** Run the parameter generation loop.
    Continuously generates parameters and sends them via HTTP POST.

    @param config Configuration for URL and interval
    @param builder Parameter builder
    @param to_json Function to convert parameter value to JSON string
*)

(** {1 Predefined parameter generators} *)

val oscillate : min:float -> max:float -> period:float -> float t
(** Create an oscillating parameter using a sine wave
    @param min Minimum value
    @param max Maximum value
    @param period Number of steps for one complete cycle
*)

val lerp : start:float -> stop:float -> steps:int -> float t
(** Create a linearly interpolating parameter that cycles
    @param start Starting value
    @param stop Ending value
    @param steps Number of steps for interpolation
*)

val random_walk : start:float -> step_size:float -> float t
(** Create a random walk parameter
    @param start Initial value
    @param step_size Maximum step size per iteration
*)

val cycle : 'a list -> 'a t
(** Cycle through a list of values infinitely *)
