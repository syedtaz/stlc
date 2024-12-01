open Stlc.Types
open Test_base
open Core
module M = Stlc.Stackless

let incr x = x + 1
let decr x = x - 1
let def = M.(fun { value = v; env = _ } -> Ok v)
let%test_unit "int literal" = int_literal (fun x -> M.run (TmInt x))
let%test_unit "bool literal" = bool_literal (fun x -> M.run (TmBool x))

let%test_unit "bool var" =
  bool_var (fun x -> M.run ~env:(`Specific [ "a", Bool x, TyBool ]) (TmVar 0))
;;

let%test_unit "int var" =
  int_var (fun x -> M.run ~env:(`Specific [ "a", Int x, TyBool ]) (TmVar 0))
;;

let%test_unit "incr app" = incr_app (fun x -> M.run (TmApp (TmOp incr, TmInt x)))
let%test_unit "decr app" = decr_app (fun x -> M.run (TmApp (TmOp decr, TmInt x)))

let%test_unit "incr 1" =
  incr_1 (fun x ->
    M.run
      ~cont:
        (`Specific
          (fun { value = v; env = e } ->
            M.eval (TmInt x) e
            >>= fun { value = v'; env = e' } -> M.apply { a = v; b = v' } e' >>= def))
      (TmOp incr))
;;

let%test_unit "decr 1" =
  decr_1 (fun x ->
    M.run
      ~cont:
        (`Specific
          (fun { value = v; env = e } ->
            M.eval (TmInt x) e
            >>= fun { value = v'; env = e' } -> M.apply { a = v; b = v' } e' >>= def))
      (TmOp decr))
;;
