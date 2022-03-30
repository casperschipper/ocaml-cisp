open OUnit
open Result

let test_udp_send_recv packet =
  let open Lwt in
  let open Osc_lwt.Udp in
  let localhost = Unix.inet_addr_of_string "127.0.0.1" in (* local *)
  let port = 4567 in
  let addr = Lwt_unix.ADDR_INET (localhost, port) in (* an address *)
  let buffer_length = 1024 in (* max buffer length, just an int *)
  bracket (* a bracket is something that produces an object, passes it to a test, than breaks it appart *)
    (fun () ->
      Lwt_main.run (* this is a thunk, that produces a client and server *)
        (Client.create ()
        >>= (fun client -> Server.create addr buffer_length
                           >>= (fun server -> return (client, server))))) (* creaate a Client and a Server and store them in a tuple *)
    (fun (client, server) ->
      let mvar = Lwt_mvar.create_empty () in (* the mvar is used to pass a value around *)
      Lwt_main.run 
        (Lwt.async (fun () ->
          Server.recv server
          >>= Lwt_mvar.put mvar); (* this is a toplevel promise, but only executed for the sideeffect, it syncs a recv and if there is a result the callback stores it in the mvar ! *)
         Client.send client addr packet (* here is the real thing, it sends the packet, this function, when success promise that resolves with () *)
         >>= (fun () -> Lwt_mvar.take mvar (* ok, there is a result, lets see what is in there *)
        >>= (function
          | Ok (received_packet, _) ->
            Test_common.assert_packets_equal
              packet
              received_packet;
            return ()
          | Error `Missing_typetag_string ->
            Lwt.fail (Failure "Missing typetag string")
          | Error (`Unsupported_typetag tag) ->
            Lwt.fail (Failure (Printf.sprintf "Unsupported typetag: %c" tag))))))
    (fun (client, server) -> (* destroy client and destroy server *)
      Lwt_main.run
        (Client.destroy client
        >>= (fun () -> Server.destroy server)))
    ()

let udp_send_recv =
  "udp_send_recv" >::: (
    List.map
      (fun (name, packet) -> 
        name >:: (fun () -> test_udp_send_recv packet))
      Test_common.test_packets_internal
  )

let suite =
  "lwt_suite" >:::
    [
      udp_send_recv;
    ]

let () =
  print_endline "-------- Lwt tests --------";
  Test_common.run suite
