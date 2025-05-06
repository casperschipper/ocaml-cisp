open Cisp

module S = Lo.Server

type state =
  State of { pitch : int }

let state = ref (State { pitch = 0 })
  
let getPitch (State { pitch }) = pitch
  
let myHandler path data =
  match path with
  | "/pitch" ->
     begin
       let data = Array.to_list data in
       let parse msg =
         match msg with 
         |`Int32 n -> Some n
         | _ -> None
       in
       let optP =
         match data with
         | [msg] -> parse msg
         | _ -> None
       in
       match optP with
       | Some pitch ->
          begin
            state := State { pitch = pitch }
          end
       | None -> ()
     end
  | _ ->
     ()

let receive_osc_thread () =
  let port = 7777 in
  let s = S.create port myHandler in
  while true do
    S.recv s
  done

let audio_thread () =
  let psq =
    Cisp.ofRef state |> Seq.map getPitch
  in
  let mySeq =
    Cisp.hold psq (Cisp.seq [ (-1.0); 1.0 ])
  in
  Jack.playSeqs 1 Process.sample_rate [ mySeq ]

let () =
  let t1 = Thread.create receive_osc_thread () in
  let t2 = Thread.create audio_thread () in
  List.iter Thread.join [t1;t2]
 
