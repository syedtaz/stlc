open Stlc.Types
open Test_base
open Core
module M = Stlc.Higher_order

let incr x = x + 1
let decr x = x - 1

let def_dump s =
  match s with
  | [] -> Ok (Unit, TyUnit)
  | (h, typ) :: _ -> Ok (h, typ)
;;

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
          (fun { stack = s; env = e; dump = d; tail = _ } ->
            M.run_a
              { stack = s
              ; env = e
              ; ctl = (fun { stack = s'; env = _; dump = d'; tail = _ } -> d' s')
              ; dump = d
              ; tail = true
              }))
      (TmOp incr))
;;

let%test_unit "decr 1" =
  decr_1 (fun x ->
    M.run
      ~stack:(`Specific [ Int x, TyInt ])
      ~control:
        (`Specific
          (fun { stack = s; env = e; dump = d; tail = _ } ->
            M.run_a
              { stack = s
              ; env = e
              ; ctl = (fun { stack = s'; env = _; dump = d'; tail = _ } -> d' s')
              ; dump = d
              ; tail = true
              }))
      (TmOp decr))
;;

let%test_unit "incr app" = incr_app (fun x -> M.run (TmApp (TmOp incr, TmInt x)))
let%test_unit "decr app" = decr_app (fun x -> M.run (TmApp (TmOp decr, TmInt x)))

let%test_unit "function return" =
  function_return (fun x ->
    M.run
      ~dump:
        (`Specific
          (fun s ->
            M.run_t
              (TmOp decr)
              { stack = s
              ; env = []
              ; ctl =
                  (fun { stack = s; env = e; dump = d; tail = _ } ->
                    M.run_a
                      { stack = s
                      ; env = e
                      ; ctl = (fun { stack = s'; env = _; dump = d'; tail = _ } -> d' s')
                      ; dump = d
                      ; tail = false
                      })
              ; dump = def_dump
              ; tail = true
              }))
      (TmInt x))
;;

let%test_unit "abstract and return" =
  abstract_and_return (fun x ->
    M.run
      ~control:
        (`Specific
          (fun { stack = s; env = e; dump = d; tail = _ } ->
            M.run_t
              (TmAbs ("x", TyInt, TmVar 0))
              { stack = s
              ; env = e
              ; ctl =
                  (fun { stack = s'; env = e'; dump = d'; tail = _ } ->
                    M.run_a
                      { stack = s'
                      ; env = e'
                      ; ctl =
                          (fun { stack = s''; env = _; dump = d''; tail = _ } -> d'' s'')
                      ; dump = d'
                      ; tail = false
                      })
              ; dump = d
              ; tail = true
              }))
      ~dump:
        (`Specific
          (fun s ->
            M.run_t
              (TmOp decr)
              { stack = s
              ; env = []
              ; ctl =
                  (fun { stack = s'; env = e'; dump = d'; tail = _ } ->
                    M.run_a
                      { stack = s'
                      ; env = e'
                      ; ctl =
                          (fun { stack = s''; env = _; dump = d''; tail = _ } -> d'' s'')
                      ; dump = d'
                      ; tail = false
                      })
              ; dump = def_dump
              ; tail = true
              }))
      (TmInt x))
;;

let%test_unit "abstract and apply" =
  abstract_and_apply (fun x ->
    M.run
      ~control:
        (`Specific
          (fun { stack = s; env = e; dump = d; tail = _ } ->
            M.run_t
              (TmAbs ("f", TyArr (TyInt, TyInt), TmApp (TmVar 0, TmInt x)))
              { stack = s
              ; env = e
              ; ctl =
                  (fun { stack = s'; env = e'; dump = d'; tail = _ } ->
                    M.run_a
                      { stack = s'
                      ; env = e'
                      ; ctl =
                          (fun { stack = s''; env = _; dump = d''; tail = _ } -> d'' s'')
                      ; dump = d'
                      ; tail = false
                      })
              ; dump = d
              ; tail = true
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
          (fun { stack = s; env = e; dump = d; tail = _ } ->
            M.run_t
              (TmInt x)
              { stack = s
              ; env = e
              ; ctl =
                  (fun { stack = s'; env = e'; dump = d'; tail = _ } ->
                    M.run_t
                      TmPair
                      { stack = s'
                      ; env = e'
                      ; ctl =
                          (fun { stack = s''; env = e''; dump = d''; tail = _ } ->
                            M.run_t
                              TmFst
                              { stack = s''
                              ; env = e''
                              ; ctl =
                                  (fun { stack = s'''; env = _; dump = d'''; tail = _ } ->
                                    d''' s''')
                              ; dump = d''
                              ; tail = false
                              })
                      ; dump = d'
                      ; tail = false
                      })
              ; dump = d
              ; tail = true
              }))
      (TmInt x))
;;
