open Seq
open Lwt.Syntax

let play ip port oscAddress timer values =
  let control = Cisp.zip values timer in
  let client = Osc_lwt.Udp.Client.create () in
  let addr = Unix.ADDR_INET (Unix.inet_addr_of_string ip, port) in
  let rec tic event =
    match event () with
    | Cons( (v, del), tail ) ->
       let* () = Lwt_unix.sleep del in
       let* () = Lwt.bind client (fun c ->
                     Osc_lwt.Udp.Client.send c addr (Message {address = oscAddress; arguments = [Osc.OscTypes.Float32 v]}))
       in (tic tail)
    | Nil ->
       Lwt_io.printl "stream has ended"
  in
  tic control
  


     
  
