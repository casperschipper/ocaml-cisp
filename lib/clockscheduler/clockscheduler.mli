(** Clockscheduler: An interface for scheduling tasks based on clock events. *)

(** [t] is the type representing a clock scheduler instance. *)
type 'a clockscheduler

type ntp = Ntp of Int64.t

val unix_to_ntp_timestamp : float -> ntp

val u32_to_float : Int32.t -> float

val ntp64_to_seconds : Int32.t * Int32.t -> float

val update : (float -> 'a -> unit) -> 'a clockscheduler -> 'a clockscheduler

val create :
     interval:float
  -> overlap:float
  -> latency:float
  -> seq:(float * 'a) Infseq.t
  -> max_events_per_buffer:int
  -> 'a clockscheduler

val cisp_schedular :
  float -> (float * 'a) Infseq.t -> 'a clockscheduler

