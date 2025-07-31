(* === Utility Functions === *)

let pad4 s =
  let len = String.length s in
  let pad = (4 - (len mod 4)) mod 4 in
  s ^ String.make pad '\000'

let encode_osc_string s = Bytes.of_string (pad4 s)

let encode_osc_int32 n =
  let b = Bytes.create 4 in
  EndianBytes.BigEndian.set_int32 b 0 n ;
  b

let encode_osc_float f =
  let b = Bytes.create 4 in
  EndianBytes.BigEndian.set_int32 b 0 (Int32.bits_of_float f) ;
  b

(* === OSC Message Encoder === *)

type osc_arg = I of int | F of float | S of string

let pad4 s =
  let len = String.length s in
  let pad = (4 - (len mod 4)) mod 4 in
  s ^ String.make pad '\000'

let encode_osc_string s = Bytes.of_string (pad4 s)

let encode_osc_int32 n =
  let b = Bytes.create 4 in
  EndianBytes.BigEndian.set_int32 b 0 n;
  b

let encode_osc_float f =
  let b = Bytes.create 4 in
  EndianBytes.BigEndian.set_int32 b 0 (Int32.bits_of_float f);
  b

type osc_arg =
  | I of int
  | F of float
  | S of string

let encode_osc_message ~address ~args =
  let b = Buffer.create 256 in

  (* Address *)
  Buffer.add_bytes b (encode_osc_string address);

  (* Typetags *)
  let tag_str =
    "," ^ String.concat "" (List.map (function
      | I _ -> "i"
      | F _ -> "f"
      | S _ -> "s"
    ) args)
  in
  Buffer.add_bytes b (encode_osc_string tag_str);

  (* Arguments *)
  List.iter (function
    | I n -> Buffer.add_bytes b (encode_osc_int32 (Int32.of_int n))
    | F f -> Buffer.add_bytes b (encode_osc_float f)
    | S s -> Buffer.add_bytes b (encode_osc_string s)
  ) args;

  Buffer.contents b |> Bytes.of_string


(* === Bundle Encoder === *)

let ntp_time_of_unix t =
  let offset = 2208988800.0 in
  let t = t +. offset in
  let int_part = Int64.of_float (floor t) in
  let frac_part = Int64.of_float ((t -. floor t) *. 4294967296.0) in
  Int64.(shift_left int_part 32 |> logor frac_part)

let encode_osc_bundle ~time ~messages =
  let b = Buffer.create 256 in
  Buffer.add_bytes b (encode_osc_string "#bundle") ;
  let ntp_time = ntp_time_of_unix time in
  let tbuf = Bytes.create 8 in
  EndianBytes.BigEndian.set_int64 tbuf 0 ntp_time ;
  Buffer.add_bytes b tbuf ;
  List.iter
    (fun msg ->
      let len = Bytes.length msg in
      let lenbuf = Bytes.create 4 in
      EndianBytes.BigEndian.set_int32 lenbuf 0 (Int32.of_int len) ;
      Buffer.add_bytes b lenbuf ;
      Buffer.add_bytes b msg )
    messages ;
  Buffer.contents b |> Bytes.of_string

(* === UDP Sender === *)

let print_hex_bytes bytes =
  Bytes.iter (fun c -> Printf.printf "%02X " (Char.code c)) bytes ;
  print_newline ()

let send_udp ~ip ~port ~bytes =
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_DGRAM 0 in
  let addr = Unix.inet_addr_of_string ip in
  let target = Unix.ADDR_INET (addr, port) in
  ignore (Unix.sendto sock bytes 0 (Bytes.length bytes) [] target) ;
  Unix.close sock

(* === Main === *)

let () =
  (* Parameters *)
  let synth_name = "simple" in
  let synth_id = 1000 in
  let add_action = 1 in
  let target = 0 in
  let freq = 440.0 in
  let dur = 1.0 in
  (* OSC args for /s_new *)
  let args =
    [ S synth_name
    ; I synth_id
    ; I add_action
    ; I target
    ; S "freq"
    ; F freq
    ; S "dur"
    ; F dur ]
  in
  let msg = encode_osc_message ~address:"/s_new" ~args in
  let bundle_time = Unix.gettimeofday () +. 1.0 in
  let bundle = encode_osc_bundle ~time:bundle_time ~messages:[msg] in
  print_hex_bytes bundle;
  send_udp ~ip:"127.0.0.1" ~port:57110 ~bytes:bundle ;
  Printf.printf "Sent /s_new for SynthDef '%s' to SuperCollider at %.3f\n%!"
    synth_name bundle_time
