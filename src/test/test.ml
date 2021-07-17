open OUnit2
open Midi
open Cisp

(* 
let rec foo () =
let* fish = Lwt_unix.sleep 3.0 in
let* boo = Lwt_io.printl "tick" in
foo ();;
 *)

(*open Midi*)
(* 
let empty_list = []

let list_a = [1; 2; 3]

let test_list_length _ =
  (* Check if the list is empty. *)
  assert_equal 0 (List.length empty_list) ;
  (* Check if a given list contains 3 elements. *)
  assert_equal 3 (List.length list_a)

let test_list_append _ =
  let list_b = List.append empty_list [1; 2; 3] in
  assert_equal list_b list_a

let suite =
  "ExampleTestList"
  >::: [ "test_list_length" >:: test_list_length
       ; "test_list_append" >:: test_list_append ] *)

let noteA = mkDelayedNote 3 c3

let noteB = mkDelayedNote 5 (transP 2 c3)

let myC4 = transP 36 c3

let noteC = mkDelayedNote 0 myC4

let myArp = List.fold_left insertNoteInScore emptyScore [noteB; noteA; noteC]

let testBundle =
  let bundleA = soloBundle myC4 in
  let bundleB = soloBundle c3 in
  mergeBundles bundleA bundleB

let test_arp _ =
  let sqArp () = Seq.Cons (Some myArp, st None) in
  let played = playArp (sqArp |> take 30) in
  let first = Cisp.head played |> Option.map getFirstOfBundle in
  let _ =
    let f opt = print_string "playing: " ; opt |> printBundle in
    Seq.iter f played ; flush stdout
  in
  let opt = match first with Some _ -> true | _ -> false in
  assert_equal true opt

(*let test_mutate _ =
  let open Seq in
  let arr = [|1; 2; 3; 4; 5; 6; 7|] |> Array.map float_of_int in
  let idx = count |> map (fun x -> x mod Array.length arr) in
  let value = seq [99; 101] |> floatify in
  let wrIdx = countFrom 1 |> map (fun x -> x mod Array.length arr) in
  let writer = zip value wrIdx |> map Option.some in
  let ctrl = makeMutateArray idx writer in
  let tst = mutateArray ctrl arr in
  let _ =
    Seq.iter (fun x -> print_string " " ; print_float x) (tst |> take 3) ;
    flush stdout
  in
  assert_equal true true*)

let test_mutate_effect _ =
  let open Seq in
  let arr = [|1; 2; 3|] in
  let idx = wrappedCount arr in
  let value = seq [99; 101; 103; 104] in
  let clock = interval (st 10) in
  let writer =
    zip idx value
    |> map (fun (i, v) -> writeOne arr i v ; ())
    |> syncEffectClock clock
  in
  let out = effectSync writer (index arr idx) in
  let result = take 30 out in
  let _ = Seq.iter (fun x -> print_string " " ; print_int x) result in
  assert_equal true true

let suite =
  "test delNote"
  >::: ["test_arp" >:: test_arp; "test_mutate" >:: test_mutate_effect]


let rawMidi =
  

let () = run_test_tt_main suite
