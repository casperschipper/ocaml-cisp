type 'a timedRecord  =
  TimedRecord of { t : int ; v : 'a }

type 'a historyArray = 
  History of { mutable index : int ; size : int; records : 'a timedRecord Array.t ; mutable last_record : int}

let record_model_controls timerGenerator (History history) anything =
  let t = timerGenerator () in
  let currentIndex = history.index in
  let () = history.index <- currentIndex + 1 in
 
  if currentIndex < history.size then
    let arr = history.records in
    arr.(currentIndex) <- TimedRecord { t; v = anything };
    history.last_record <- currentIndex
  else
    ()

(* this function needs to be played back at real time (so use the most updated clock in the system) *)
let playback clock history = 
  let then = clock () in 
  let i = 0 in
  let rec aux currenti in
    if currenti > history.last_record then
      Seq.Nil
    else
      let TimedRecord { t; v } = history.records.(currenti) in
      let now = clock () in
      let delay = t - (now - then) in
      if delay > 0 then v ();
      Seq.Cons (v, fun () -> aux (currenti + 1))
  in
  aux i


  