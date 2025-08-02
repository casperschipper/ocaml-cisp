

type event = {frequency: float; duration: float}

let test_event = {frequency= 440.0; duration= 0.01}

let from_event_to_bundle start_time {frequency; duration} =
  Supercollider.simple_tone ~time:start_time ~freq:frequency ~dur:duration

let send_osc sender time evt =
  let bytes = from_event_to_bundle time evt in
  Supercollider.send_message sender bytes

let streamed_sched =
  let scheduler_interval = 0.5 in
  let samples = !Process.sample_rate *. scheduler_interval |> floor |> int_of_float in
  let event_sq = Infseq.repeat (0.01, test_event) in
  let sched =
    Clockscheduler.create ~interval:0.5 ~overlap:1.25 event_sq 10000
  in
  let sender = Supercollider.init_sender ~ip:"127.0.0.1" ~port:57110 in
  let create_event time event = send_osc sender time event in
  let sched_sq =
    Cisp.simpleRecursive sched (Clockscheduler.update create_event) (fun x ->
        ignore x )
  in
  Cisp.hold (Cisp.st samples) sched_sq

let _ =
  Jack.playSeqs 0 Process.sample_rate
    [Cisp.syncEffect (Cisp.st 0.0) streamed_sched]
