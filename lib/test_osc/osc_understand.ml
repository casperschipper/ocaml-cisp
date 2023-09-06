open OUnit
open Result
open Osc.OscTypes


let message_no_args = Osc.OscTypes.(Message {
  address = "/test";
  arguments = [];
})

let message_empty_string_arg = Osc.OscTypes.(Message {
  address = "/test";
  arguments = [
    String "";
  ];
})

let message_all_args_basic = Osc.OscTypes.(Message {
  address = "/test";
  arguments = [
    String "foobar";
    Int32 123l;
    Float32 456.789;
  ];
})

let message_empty_blob_arg = Osc.OscTypes.(Message {
  address = "/test";
  arguments = [
    Blob "";
  ];
})

let message_timetag_immediate_arg = Osc.OscTypes.(Message {
  address = "/test";
  arguments = [
    Timetag Immediate;
  ];
})

let message_timetag_time_arg = Osc.OscTypes.(Message {
  address = "/test";
  arguments = [
    Timetag (Time {seconds = 456l; fraction = 123l});
  ];
})

let message_all_args = Osc.OscTypes.(Message {
  address = "/test";
  arguments = [
    Blob "baz";
    String "quux";
    Int32 789l;
    Float32 123.456;
    Timetag Immediate;
    Timetag (Time {seconds = 456l; fraction = 123l});
  ];
})

let bundle_immediate_no_packets = Osc.OscTypes.(Bundle {
  timetag = Immediate;
  packets = [];
})

let bundle_no_packets = Osc.OscTypes.(Bundle {
  timetag = Time {seconds = 147l; fraction = 258l};
  packets = [];
})

let bundle_one_message = Osc.OscTypes.(Bundle {
  timetag = Time {seconds = 12l; fraction = 48l};
  packets = [message_all_args];
})

let bundle_two_messages = Osc.OscTypes.(Bundle {
  timetag = Immediate;
  packets = [message_all_args; message_empty_blob_arg];
})

let bundle_recursive = Osc.OscTypes.(Bundle {
  timetag = Time {seconds = 678l; fraction = 345l};
  packets = [message_all_args; bundle_two_messages];
})

let test_packets_basic = [
  "message_no_args", message_no_args;
  "message_empty_string_arg", message_empty_string_arg;
  "message_all_args_basic", message_all_args_basic;
]

let test_packets_extended = [
  "message_empty_blob_arg", message_empty_blob_arg;
  "message_timetag_immediate_arg", message_timetag_immediate_arg;
  "message_timetag_time_arg", message_timetag_time_arg;
  "message_all_args", message_all_args;
]

let test_bundles = [
  "bundle_immediate_no_packets", bundle_immediate_no_packets;
  "bundle_no_packets", bundle_no_packets;
  "bundle_one_message", bundle_one_message;
  "bundle_two_messages", bundle_two_messages;
  "bundle_recursive", bundle_recursive;
]

let test_packets_sclang = test_packets_basic

let test_packets_internal =
  test_packets_basic @ test_packets_extended @ test_bundles

let are_arguments_equal arg1 arg2 =
  let open Osc.OscTypes in
  match arg1, arg2 with
  | Blob a, Blob b -> a = b
  | String a, String b -> a = b
  | Int32 a, Int32 b -> a = b
  | Float32 a, Float32 b ->
    let diff = abs_float (a -. b) in
    diff <= 0.01
  | Timetag a, Timetag b -> a = b
  | _, _ -> false

let string_of_argument = function
  | Osc.OscTypes.Blob s -> Printf.sprintf "Blob %s" s
  | Osc.OscTypes.String s -> Printf.sprintf "String %s" s
  | Osc.OscTypes.Int32 i -> Printf.sprintf "Int32 %ld" i
  | Osc.OscTypes.Float32 f -> Printf.sprintf "Float32 %f" f
  | Osc.OscTypes.Timetag t ->
    Printf.sprintf "Timetag %s"
      (match t with
      | Osc.OscTypes.Immediate -> "Immediate"
      | Osc.OscTypes.Time time ->
        Printf.sprintf "%ld.%ld"
          time.seconds time.fraction)

let assert_messages_equal message1 message2 =
  let open Osc.OscTypes in
  assert_equal
    ~msg:"Incorrect address"
    message1.address message2.address;
  assert_equal
    ~msg:"Incorrect number of arguments"
    (List.length message1.arguments)
    (List.length message2.arguments);
  List.iter2
    (fun argument1 argument2 ->
      assert_equal
        ~cmp:are_arguments_equal
        ~printer:string_of_argument
        argument1 argument2)
    message1.arguments
    message2.arguments

let rec assert_bundles_equal bundle1 bundle2 =
  let open Osc.OscTypes in
  assert_equal
    ~msg:"Incorrect timetag"
    bundle1.timetag bundle2.timetag;
  assert_equal
    ~msg:"Wrong number of packets in bundle"
    (List.length bundle1.packets)
    (List.length bundle2.packets);
  List.iter2
    (fun packet1 packet2 -> assert_packets_equal packet1 packet2)
    bundle1.packets bundle2.packets

and assert_packets_equal packet1 packet2 =
  let open Osc.OscTypes in
  match packet1, packet2 with
  | Message message1, Message message2 ->
    assert_messages_equal message1 message2
  | Bundle bundle1, Bundle bundle2 ->
    assert_bundles_equal bundle1 bundle2
  | _, _ ->
    assert_failure "Packet types differ"

let check_results results =
  if List.exists
    OUnit.(function | RFailure _ | RError _ -> true | _ -> false)
    results
  then exit 1
  else exit 0

let run suite =
  check_results (run_test_tt suite)

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
            assert_packets_equal
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
      test_packets_internal
  )

let suite =
  "lwt_suite" >:::
    [
      udp_send_recv;
    ]

let () =
  print_endline "-------- Lwt tests --------";
  run suite



