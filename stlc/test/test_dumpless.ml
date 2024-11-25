open Stlc.Types

open Test_base
module M = Stlc.Dumpless

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

let%test_unit "incr 1" =
  incr_1 (fun x ->
    M.run
      ~stack:(`Specific [ Int x, TyInt ])
      ~control:
        (`Specific
          (fun { stack = s; env = e } ->
            M.apply { stack = s; env = e; control = (fun { stack = s'; _ } -> Ok s') }))
      (TmOp incr))
;;

let%test_unit "decr 1" =
  decr_1 (fun x ->
    M.run
      ~stack:(`Specific [ Int x, TyInt ])
      ~control:
        (`Specific
          (fun { stack = s; env = e } ->
            M.apply { stack = s; env = e; control = (fun { stack = s'; _ } -> Ok s') }))
      (TmOp decr))
;;

let%test_unit "incr app" = incr_app (fun x -> M.run (TmApp (TmOp incr, TmInt x)))
let%test_unit "decr app" = decr_app (fun x -> M.run (TmApp (TmOp decr, TmInt x)))

(* CPS monad maybe lol *)
let%test_unit "abstract and return" =
  abstract_and_return (fun x ->
    M.run
      ~control:
        (`Specific
          (fun { stack = s; env = e } ->
            M.eval
              (TmAbs ("x", TyInt, TmVar 0))
              { stack = s
              ; env = e
              ; control =
                  (fun { stack = s'; env = e' } ->
                    M.apply
                      { stack = s'
                      ; env = e'
                      ; control =
                          (fun { stack = s''; env = e'' } ->
                            M.eval
                              (TmOp decr)
                              { stack = s''
                              ; env = e''
                              ; control =
                                  (fun { stack = s'''; env = e''' } ->
                                    M.apply
                                      { stack = s'''
                                      ; env = e'''
                                      ; control =
                                          (fun { stack = s''''; env = _ } -> Ok s'''')
                                      })
                              })
                      })
              }))
      (TmInt x))
;;

let%test_unit "abstract and apply" =
  abstract_and_apply (fun x ->
    M.run
      ~control:
        (`Specific
          (fun { stack = s; env = e } ->
            M.eval
              (TmAbs ("f", TyArr (TyInt, TyInt), TmApp (TmVar 0, TmInt x)))
              { stack = s
              ; env = e
              ; control =
                  (fun { stack = s'; env = e' } ->
                    M.apply
                      { stack = s'
                      ; env = e'
                      ; control = (fun { stack = s''; env = _ } -> Ok s'')
                      })
              }))
      (TmOp decr))
;;

let%test_unit "project 1" =
  project_1 (fun x ->
    M.run ~stack:(`Specific [ Pair (Int (x + 1), Int x), TyPair (TyInt, TyInt) ]) TmFst)
;;

let%test_unit "project 2" =
  project_2 (fun x ->
    M.run ~stack:(`Specific [ Pair (Int x, Int (x + 1)), TyPair (TyInt, TyInt) ]) TmSnd)
;;

let%test_unit "construct pair" =
  construct_pair (fun x ->
    M.run
      ~control:
        (`Specific
          (fun { stack = s; env = e } ->
            M.eval
              (TmInt x)
              { stack = s
              ; env = e
              ; control =
                  (fun { stack = s'; env = e' } ->
                    M.eval
                      TmPair
                      { stack = s'
                      ; env = e'
                      ; control =
                          (fun { stack = s''; env = e'' } ->
                            M.eval
                              TmFst
                              { stack = s''
                              ; env = e''
                              ; control = (fun { stack = s'''; _ } -> Ok s''')
                              })
                      })
              }))
      (TmInt x))
;;
