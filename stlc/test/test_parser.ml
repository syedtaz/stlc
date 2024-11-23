open Stlc.Parser
open Stlc.Newtypes.Term

let pp x =
  match x with
  | Ok res -> Format.sprintf "Ok (%s)\n" (show res)
  | Error rest -> Format.sprintf "Error (%s)" rest
;;

let%expect_test "λx.incr x" =
  let res = parse "incr" in
  Format.print_string (pp res);
  [%expect {| Ok (TmPrim(incr)) |}]
;;

let%expect_test "λx.decr x" =
  let res = parse "decr" in
  Format.print_string (pp res);
  [%expect {| Ok (TmPrim(decr)) |}]
;;

let%expect_test "λx.()" =
  let res = parse "λx.()" in
  Format.print_string (pp res);
  [%expect {| Ok (TmUnit) |}]
;;

(* let%expect_test "λx.(x)" =
  let res = parse "λx.(x)" in
  Format.print_string (pp res);
  [%expect {| Ok (TmVar 0) |}]
;; *)