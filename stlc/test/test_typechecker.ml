open Stlc.Typechecker

let%expect_test "show_int" =
  print_endline (show_ty TyInt);
  [%expect {| int |}]
;;

let%expect_test "show_arr" =
  print_endline (show_ty (TyArr (TyInt, TyInt)));
  [%expect {| int -> int |}]
;;

let%expect_test "typeof TmInt" =
  let ty = typeof [] (TmInt 2) in
  print_endline (show_ty ty);
  [%expect {| int |}]
;;

let%expect_test "typeof TmVar with 1 element" =
  let ty = typeof [ "a", VarBind TyInt ] (TmVar 0) in
  print_endline (show_ty ty);
  [%expect {| int |}]
;;

let%expect_test "typeof TmVar with 1 element" =
  let ty = typeof [ "a", VarBind TyInt; "b", VarBind TyInt ] (TmVar 1) in
  print_endline (show_ty ty);
  [%expect {| int |}]
;;
