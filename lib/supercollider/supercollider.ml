(* supercollider.ml *)

(* === Encoding utilities === *)

let encode_osc_string s =
  let s_with_null = s ^ "\000" in
  let len = String.length s_with_null in
  let pad = (4 - (len mod 4)) mod 4 in
  let total = s_with_null ^ String.make pad '\000' in
  Bytes.of_string total

let encode_osc_int32 n =
  let b = Bytes.create 4 in
  EndianBytes.BigEndian.set_int32 b 0 n ;
  b

let encode_osc_float f =
  let i = Int32.bits_of_float f in
  let b = Bytes.create 4 in
  EndianBytes.BigEndian.set_int32 b 0 i ;
  b

(* === OSC message construction === *)

type osc_arg = I of int | F of float | S of string

let encode_osc_message ~address ~args =
  let b = Buffer.create 256 in
  Buffer.add_bytes b (encode_osc_string address) ;
  let typetag =
    ","
    ^ String.concat ""
        (List.map
           (function
             | I _ -> "i"
             | F _ -> "f"
             | S _ -> "s" )
           args )
  in
  Buffer.add_bytes b (encode_osc_string typetag) ;
  List.iter
    (function
      | I n -> Buffer.add_bytes b (encode_osc_int32 (Int32.of_int n))
      | F f -> Buffer.add_bytes b (encode_osc_float f)
      | S s -> Buffer.add_bytes b (encode_osc_string s) )
    args ;
  Buffer.contents b |> Bytes.of_string

(* === OSC bundle construction === *)

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

(* === UDP sender type === *)

type sender = {sock: Unix.file_descr; addr: Unix.sockaddr}

let init_sender ~ip ~port =
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_DGRAM 0 in
  let addr = Unix.inet_addr_of_string ip in
  let sockaddr = Unix.ADDR_INET (addr, port) in
  {sock; addr= sockaddr}

let close_sender sender = Unix.close sender.sock

let send_message sender msg =
  ignore (Unix.sendto sender.sock msg 0 (Bytes.length msg) [] sender.addr)

let send_bundle sender ~time ~messages =
  let bundle = encode_osc_bundle ~time ~messages in
  send_message sender bundle

let send_synth absolute_time sender ~name ~synth_id ~add_action ~target ~params
    =
  let args =
    [S name; I synth_id; I add_action; I target]
    @ List.flatten (List.map (fun (k, v) -> [S k; F v]) params)
  in
  let msg = encode_osc_message ~address:"/s_new" ~args in
  let time = absolute_time in
  send_bundle sender ~time ~messages:[msg]

type newSynth =
  | NewSynth of
      { synth_name: string
      ; synth_id: int
      ; add_action: int
      ; target: int
      ; params: (string * osc_arg) list }

let simple_synth_with_pars parameters =
  NewSynth
    { synth_name= "simple"
    ; synth_id= -1
    ; add_action= 1
    ; target= 0
    ; params= parameters }

let params_to_bytes params =
  params |> List.concat_map (fun (name, value) -> [S name; value])

let to_args (NewSynth {synth_name; synth_id; add_action; target; params}) =
  [ S synth_name
  ; (* SynthDef name *)
    I synth_id
  ; (* Synth ID *)
    I add_action
  ; (* Add action *)
    I target (* Target group *) ]
  @ params_to_bytes params

let simple_tone ~time:start_t ~freq:freq ~dur:dur ~pos:pos =
  let args =
    simple_synth_with_pars [("freq", F freq); ("dur", F dur); ("pos", F pos)] |> to_args
  in
  let message = encode_osc_message ~address:"/s_new" ~args in
  encode_osc_bundle ~time:start_t ~messages:[message]

let send_multiple_synths absolute_time sender synths =
  let messages =
    List.map
      (fun (name, synth_id, add_action, target, params) ->
        let args =
          [S name; I synth_id; I add_action; I target]
          @ List.flatten (List.map (fun (k, v) -> [S k; F v]) params)
        in
        encode_osc_message ~address:"/s_new" ~args )
      synths
  in
  send_bundle sender ~time:absolute_time ~messages
