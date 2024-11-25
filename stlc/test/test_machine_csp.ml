open Stlc.Types

module CheckMachine (M : Stlc.Machine_csp_intf.Intf) = struct
  open Test_base
  open Core

  let incr x = x + 1
  let decr x = x - 1

  let%test_unit "int literal" =
    int_literal (fun x -> M.run ~debug:false (TmInt x) @@ M.init [] [])
  ;;

  let%test_unit "bool literal" =
    bool_literal (fun x -> M.run ~debug:false (TmBool x) @@ M.init [] [])
  ;;

  let%test_unit "bool var" =
    bool_var (fun x -> M.run ~debug:false (TmVar 0) @@ M.init [] [ "a", Bool x, TyBool ])
  ;;

  let%test_unit "int var" =
    int_var (fun x -> M.run ~debug:false (TmVar 0) @@ M.init [] [ "a", Int x, TyInt ])
  ;;

  (* let%test_unit "incr 1" =
    incr_1 (fun x -> M.run ~debug:false (TmOp incr) @@ M.init [ Int x, TyInt ] [])
  ;;

  let%test_unit "decr 1" =
    decr_1 (fun x -> M.run ~debug:false (TmOp decr) @@ M.init [ Int x, TyInt ] [])
  ;; *)

  let%test_unit "incr app" =
    incr_app (fun x -> M.run ~debug:false (TmApp (TmOp incr, TmInt x)) @@ M.init [] [])
  ;;

  (* let%expect_test "test" =
    let _ = Format.printf "%d\n" (Option.value_exn @@ extract_int @@ M.run ~debug:true  (TmOp incr) @@ M.init ~c:(fun (s, e, d) -> M.run_a ~debug:true { stack = s; env = e; dump = d}) [Int 0, TyInt] []) in
    ()
  ;; *)

  (*
     let%test_unit "incr from control" =

     let%test_unit "decr from control" = *)
end

module Csp = CheckMachine (Stlc.Csp)
