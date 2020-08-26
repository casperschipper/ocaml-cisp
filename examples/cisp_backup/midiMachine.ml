(* this is a module for a looping machine *)

open Midi

type t = {buffer: int Array.t ref; index: int}

let empty buffRef = {buffer= buffRef; index= 0}

let getIndex m = m.index

let write m rawMidiMsg =
  let idx = m.index in
  let size = Array.length !(m.buffer) in
  let newM = {m with index= (m.index + 3) mod size} in
  let () =
    match rawMidiMsg with
    | st, d1, d2 ->
        !(newM.buffer).(idx) <- st ;
        !(newM.buffer).(idx + 1) <- d1 ;
        !(newM.buffer).(idx + 2) <- d2
  in
  newM

(* let valueAt m i = *)
