open Stlc.Types
open Test_base
module M = Stlc.Controlless

let incr x = x + 1
let decr x = x - 1
let%test_unit "int literal" = int_literal (fun x -> M.run (TmInt x))
let%test_unit "bool literal" = bool_literal (fun x -> M.run (TmBool x))

let%test_unit "bool var" =
  bool_var (fun x -> M.run ~env:(`Specific [ "a", Bool x, TyBool ]) (TmVar 0))
;;

let%test_unit "int var" =
  int_var (fun x -> M.run ~env:(`Specific [ "a", Int x, TyBool ]) (TmVar 0))
;;

let%test_unit "incr_1" =
  incr_1 (fun x -> M.run ~stack:(`Specific [ Int x, TyInt ]) (TmOp incr))
;;
