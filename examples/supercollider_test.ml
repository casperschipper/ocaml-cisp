type event = {frequency: float; duration: float}

let test_event () =
  let f = Toolkit.rvfi 0.0 130.0 |> Cisp.mtof in
  let l = Toolkit.rvi 1 60 |> float_of_int in
  {frequency= f; duration= l /. f}

let from_event_to_bundle start_time {frequency; duration} =
  Supercollider.simple_tone ~time:start_time ~freq:frequency ~dur:duration ~pos:0.0

let send_osc sender time evt =
  let bytes = from_event_to_bundle time evt in
  Supercollider.send_message sender bytes

let streamed_sched =
  let scheduler_samps = 2048 in
  let scheduler_sec = Cisp.seconds_from_samples scheduler_samps in
  let event_sq = Infseq.generator (fun () -> (0.01, test_event ())) in
  let sched =
    Clockscheduler.create ~interval:scheduler_sec ~overlap:1.25 ~seq:event_sq
      ~latency:0.2 ~max_events_per_buffer:10000
  in
  let sender = Supercollider.init_sender ~ip:"127.0.0.1" ~port:57110 in
  let create_event time event = send_osc sender time event in
  let sched_sq =
    Cisp.simpleRecursive sched (Clockscheduler.update create_event) ignore
  in
  Cisp.hold (Cisp.st scheduler_samps) sched_sq

let _ =
  Jack.playSeqs 0 Process.sample_rate
    [Cisp.syncEffect (Cisp.st 0.0) streamed_sched]
