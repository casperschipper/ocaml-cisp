open Midi
open Cisp
open Plugface

let load_plug fname =
  let fname = Dynlink.adapt_filename fname in
  if Sys.file_exists fname then
    try Dynlink.loadfile fname with
    | Dynlink.Error err as e ->
        print_endline ("ERROR loading plugin: " ^ Dynlink.error_message err);
        raise e
    | _ -> failwith "Unknow error while loading plugin"
  else failwith "Plugin file does not exist"

let seq () =
  load_plug "myplugin.cmxs";
  let module M = (val get_plugin () : PLUG) in
  M.pattern

let get_last_changed_date file = Unix.stat file |> fun stats -> stats.st_mtime

let dynamic_seq =
  let _ = print_endline "tick ." in
  let rec aux tail previous_stamp () =
    let new_date = get_last_changed_date "myplugin.cmxs" in
    if new_date = previous_stamp then
      match tail () with
      | Seq.Cons (h, tl) -> Seq.Cons (h, aux tl previous_stamp)
      | Seq.Nil -> Seq.Nil
    else
      let new_seq = seq () in
      match new_seq () with
      | Seq.Cons (h, tl) -> Seq.Cons (h, aux tl new_date)
      | Seq.Nil -> Seq.Nil
  in
  aux (Seq.repeat 90) (get_last_changed_date "myplugin.cmxs")

let f inp =
  (* let _ = List.iter print_string (Dynlink.all_units ()) in *)
  let pitch =
    let arr =
      [|
        (* Cisp.listWalk [| 60; 72; 65; 55 |] (Cisp.ch [| -1; 0; 0; 0; 1 |]) ();
           Cisp.listWalk [| 65; 67; 63 |] (Cisp.ch [| -1; 0; 0; 0; 1 |]) ();
           Cisp.listWalk [| 65; 67; 63 |] (Cisp.ch [| -1; 0; 0; 0; 1 |]) (); *)
        dynamic_seq;
      |]
    in
    toumani_lst [ 3; 1; 2; 2; 1; 3 ] |> index_seq arr
  in
  inp
  |> trigger (makeNoteOfInts <$> pitch <*> st 100 <*> st 2000 <*> st 1)
  |> serialize |> Seq.map toRaw

let () =
  let f () =
    Midi.playMidi f Process.sample_rate;
    while true do
      Unix.sleep 60
    done
  in
  let _ = Thread.create f () in
  let _ =
    print_int
      (Sys.command
         "jack_connect ocaml_midi:ocaml_midi_out system_midi:playback_1");
    print_int
      (Sys.command "jack_connect system_midi:capture_2 ocaml_midi:ocaml_midi_in");
    ignore (Sys.command "jack_lsp -c -A | grep ocaml")
  in
  while true do
    Unix.sleep 30
  done
