type 'a timedRecord = TimedRecord of {t: int; v: 'a} | Empty

type 'a historyArray =
  | History of
      { mutable index: int
      ; size: int
      ; records: 'a timedRecord Array.t
      ; mutable last_record: int }

let initHistory size = Array.make size Empty

let record_snapshot timerGenerator (History history) anything =
  let t = timerGenerator () in
  let currentIndex = history.index in
  let () = history.index <- currentIndex + 1 in
  if currentIndex < history.size then (
    let arr = history.records in
    arr.(currentIndex) <- TimedRecord {t; v= anything} ;
    history.last_record <- currentIndex )
  else ()

let get_values (History history) =
  history.records |> Array.to_seq |> Seq.filter_map (fun r -> match r with 
  | Empty -> None
  | TimedRecord { t ; v } -> ignore t;Some v )



(* this function needs to be played back at real time (so use the most updated clock in the system) *)
let playback (clock : unit -> int) (hist : 'a historyArray) : 'a Option.t Seq.t
    =
  let rec aux recordIndex () =
    match hist with
    | History history -> (
        let now = clock () in
        if recordIndex > history.last_record then Seq.Nil
        else
          match history.records.(recordIndex) with
          | Empty -> Seq.Nil
          | TimedRecord {t; v} ->
              if now > t then Seq.Cons (Some v, aux (recordIndex + 1))
              else Seq.Cons (None, aux (recordIndex + 1)) )
  in
  aux 0
