open Stlc.Machine
open Stlc.Types

let%expect_test "eval single literal int" =
  let instructions = [ Term (TmInt 0) ] in
  let res = run (empty instructions) in
  print_endline (show_value res);
  [%expect {| 0 : Int |}]
;;

let%expect_test "eval single literal bool" =
  let instructions = [ Term (TmBool true) ] in
  let res = run (empty instructions) in
  print_endline (show_value res);
  [%expect {| true : Bool |}]
;;

let%expect_test "eval single literal bool 2" =
  let instructions = [ Term (TmBool false) ] in
  let res = run (empty instructions) in
  print_endline (show_value res);
  [%expect {| false : Bool |}]
;;