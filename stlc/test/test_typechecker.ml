open Stlc.Types

let show = function
  | None -> "cannot typecheck"
  | Some v -> show_ty v


let%expect_test "show_int" =
  print_endline (show_ty TyInt);
  [%expect {| int |}]
;;

let%expect_test "show_bool" =
  print_endline (show_ty TyBool);
  [%expect {| bool |}]
;;

let%expect_test "show_unit" =
  print_endline (show_ty TyUnit);
  [%expect {| () |}]
;;


let%expect_test "show_arr" =
  print_endline (show_ty (TyArr (TyInt, TyInt)));
  [%expect {| int -> int |}]
;;

let%expect_test "typeof TmInt" =
  let ty = typeof [] (TmInt 2) in
  print_endline (show ty);
  [%expect {| int |}]
;;

let%expect_test "typeof TmUnit" =
  print_endline (show (typeof [] (TmUnit)));
  [%expect {| () |}];
;;

let%expect_test "typeof TmBool" =
  print_endline (show (typeof [] (TmBool true)));
  [%expect {| bool |}];
  print_endline (show (typeof [] (TmBool false)));
  [%expect {| bool |}]
;;

let%expect_test "typeof TmVar with 1 element" =
  let ty = typeof [ "a", VarBind TyInt ] (TmVar 0) in
  print_endline (show ty);
  [%expect {| int |}]
;;

let%expect_test "typeof TmVar with 2 element" =
  let ty = typeof [ "a", VarBind TyInt; "b", VarBind TyInt ] (TmVar 1) in
  print_endline (show ty);
  [%expect {| int |}]
;;