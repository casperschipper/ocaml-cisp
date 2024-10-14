open Midi
open Cisp

(* let read_first_line filename =
     try
       (* Open the file for reading *)
       let channel = open_in filename in
       (* Read the first line from the file *)
       let first_line = input_line channel in
       (* Close the file *)
       close_in channel;
       Some first_line
     with
     | Sys_error msg ->
         Printf.eprintf "Error: %s\n" msg;
         None
     | End_of_file -> None
     | ex ->
         Printf.eprintf "Error: %s\n" (Printexc.to_string ex);
         None

   let load_plug fname () =
     let _ = print_endline fname in
     if Sys.file_exists fname then
       try Dynlink.loadfile_private fname with
       | Dynlink.Error err as e ->
           print_endline ("ERROR loading plugin: " ^ Dynlink.error_message err);
           raise e
       | _ -> failwith "Unknow error while loading plugin"
     else failwith "Plugin file does not exist"

   let seq () =
     let cmxs_filename = read_first_line "../lib/myplugin/current.txt" in
     match cmxs_filename with
     | Some name ->
         load_plug name ();
         Plugface.get_plugin ()
     | None ->
         print_endline "there was an error, sorry!";
         Seq.repeat 127

   let get_last_changed_date file = Unix.stat file |> fun stats -> stats.st_mtime

   let dynamic_seq =
     let _ = print_endline "tick ." in
     let rec aux tail previous_stamp () =
       let new_date = get_last_changed_date "../lib/myplugin/current.txt" in
       let _ =
         print_float new_date;
         print_endline ""
       in
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
     aux (Seq.repeat 90) (get_last_changed_date "../lib/myplugin/current.txt") *)

let dyn_seq control initial =
  let update msg state =
    match msg with Some new_sq -> new_sq | None -> state
  in
  recursive control initial update (fun x -> x)

let a = ref (Some (Cisp.seq [ 60; 60; 62 ]))
let b = ref (Some (Cisp.seq [ 67; 67; 67; 65 ]))
let c = ref (Some (Cisp.seq [ 72; 72; 72; 74 ]))

let inbox_seq my_ref default =
  let open Seq in
  Seq.unfold
    (fun state ->
      match !my_ref with
      | Some sq -> (
          my_ref := None;
          match sq () with
          | Cons (h, tail) -> Some (h, tail)
          | Nil -> Some (default, Seq.repeat default))
      | None -> (
          match state () with
          | Cons (h, tail) -> Some (h, tail)
          | Nil -> Some (default, Seq.repeat default)))
    (Seq.repeat default)

let casper = ref 60

let duration = ref (Some (Seq.repeat 1.0))

let f inp =
  (* let _ = List.iter print_string (Dynlink.all_units ()) in *)
  let pitch =
    let arr = [| inbox_seq a 60 ; inbox_seq b 60 ; inbox_seq c 60 |] in
    toumani_lst [ 3; 1; 2; 2; 1; 3 ] |> index_seq arr
  in
  let dura =
    inbox_seq duration 1.0 |> Seq.map (fun x -> x *. 1000.0 |> int_of_float)
  in
  inp
  |> trigger (makeNoteOfInts <$> pitch <*> st 100 <*> dura <*> st 1)
  |> serialize |> Seq.map toRaw

let main () =
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
  ()
