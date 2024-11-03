open OUnit2

(* Assuming foo.ml contains a function Foo.add *)
let test_add _ =
  assert_equal 1 1

let () =
  "foo_tests" >::: [
    "test_add" >:: test_add;
  ]
  |> run_test_tt_main