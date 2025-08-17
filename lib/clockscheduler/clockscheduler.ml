(* This is based on the tale of two clocks HTML5 tutorial.
The aim is to schedule an OSC message ahead but with a very accurate NTP OSC timestamp. 

(there is still an open question how accurate gettimeofday is especially on Linux.
It seems on MacOS it is pretty good. 
Ideally, it may even be an idea to implement actual fractional numbers (1/3) (1/5), as they could be more accurate in some circumstances.
*)

type ntp = Ntp of Int64.t

let unix_to_ntp_timestamp (unix_time : float) : ntp =
  (* NTP epoch starts at January 1, 1900 
     Unix epoch starts at January 1, 1970
     Difference is 70 years = 2208988800 seconds *)
  let ntp_unix_offset = 2208988800L in
  (* Split the float into integer seconds and fractional part *)
  let seconds = Int64.of_float (floor unix_time) in
  let fractional = unix_time -. floor unix_time in
  (* Convert seconds to NTP time (add offset) *)
  let ntp_seconds = Int64.add seconds ntp_unix_offset in
  (* Convert fractional seconds to NTP fractional format
     NTP uses 32 bits for fractional seconds (1/2^32 resolution) *)
  let ntp_fraction = Int64.of_float (fractional *. 4294967296.0) in
  (* Combine: upper 32 bits = seconds, lower 32 bits = fraction *)
  let ntp_timestamp =
    Int64.logor
      (Int64.shift_left ntp_seconds 32)
      (Int64.logand ntp_fraction 0xFFFFFFFFL)
  in
  Ntp ntp_timestamp

type 'b clockscheduler =
  { interval: float
  ; latency: float
  ; overlap: float
  ; next_event_time: float
  ; next_event: 'b Option.t
  ; seq: (float * 'b) Infseq.t
  ; max_events_per_buffer: int }

let create ~interval ~overlap ~latency ~seq
    ~max_events_per_buffer =
  { interval
  ; overlap
  ; next_event_time= Unix.gettimeofday ()
  ; next_event= None
  ; seq
  ; max_events_per_buffer
  ; latency }

let cisp_schedular interval_in_sec seq = 
  create ~interval:interval_in_sec ~overlap:1.25 ~seq:seq
      ~latency:0.2 ~max_events_per_buffer:10000

(* Interpret an Int32 as unsigned, convert to float exactly up to 2^53 *)
let u32_to_float (x : Int32.t) : float =
  let open Int64 in
  to_float (logand (of_int32 x) 0xFFFF_FFFFL)

(* NTP 64-bit fixed-point -> seconds as float (NTP epoch: 1900-01-01) *)
let ntp64_to_seconds ((sec, frac) : Int32.t * Int32.t) : float =
  let s = u32_to_float sec in
  let f = ldexp (u32_to_float frac) (-32) in
  (* frac / 2^32, done exactly *)
  s +. f

let update create_event sched =
  let now = Unix.gettimeofday () in
  let window_end = now +. (sched.interval *. sched.overlap) in
  let rec loop n next_event_time next_event future_events =
    (* plays this windows events and returns the rest and the next event outside of this window *)
    if next_event_time <= window_end then (
      ( match next_event with
      | None -> ()
      | Some evt -> create_event (next_event_time +. sched.latency) evt ) ;
      match future_events () with
      | Infseq.InfCons ((delay, event), rest) ->
          if n > sched.max_events_per_buffer then
            raise (Failure "schedular overloaded with events")
          else loop (n + 1) (next_event_time +. delay) (Some event) rest )
    else (next_event_time, next_event, future_events)
  in
  let next_time, next_event, future_events2 =
    loop 0 sched.next_event_time sched.next_event sched.seq
  in
  {sched with next_event_time= next_time; seq= future_events2; next_event}
