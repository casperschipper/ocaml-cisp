

type 'b clockscheduler = {
  interval : float;
  overlap : float;
  next_event_time : float;
  next_event : 'b Option.t;
  seq : (float * 'b) Infseq.t;
  max_events_per_buffer : int;
}

let create ?(start_time=Unix.gettimeofday ()) ~interval ~overlap seq max_events_per_buffer =
  {
    interval;
    overlap;
    next_event_time = start_time;
    next_event= None;
    seq;
    max_events_per_buffer;
  }

let update create_event sched =
  let now = Unix.gettimeofday () in
  let window_end = now +. sched.interval +. sched.overlap in
  let rec loop n next_event_time next_event future_events = 
    (* plays this windows events and returns the rest and the next event outside of this window *)
    if next_event_time <= window_end then
      ((match next_event with
      | None -> ()
      | Some evt ->
        (create_event next_event_time evt)
      );
      match future_events () with
      | Infseq.InfCons((delay,event),rest) ->
        if (n > 10000) then
          raise (Failure "schedular overloaded with events")
        else
          loop (n+1) (next_event_time +. delay) (Some event) rest)
  else 
      (next_event_time,next_event,future_events)
  in
  let (next_time,next_event,future_events2) = loop 0 sched.next_event_time sched.next_event sched.seq  in
  { sched with next_event_time = next_time; seq = future_events2; next_event }

