open Stlc.Types

let extract_int v =
  match v with
  | Int x -> Some x
  | _ -> None
;;

let extract_bool v =
  match v with
  | Bool x -> Some x
  | _ -> None
;;

module CheckMachine (M : Stlc.Machine_intf.Intf) = struct
  open Core

  let%test_unit "int literal" =
    Quickcheck.test (Int.gen_incl Int.min_value Int.max_value) ~f:(fun x ->
      [%test_eq: Int.t Option.t]
        (Some x)
        (extract_int @@ M.run @@ M.init [] [] [ Term (TmInt x) ] []))
  ;;

  let%test_unit "bool literal" =
    Quickcheck.test Bool.quickcheck_generator ~f:(fun x ->
      [%test_eq: Bool.t Option.t]
        (Some x)
        (extract_bool @@ M.run @@ M.init [] [] [ Term (TmBool x) ] []))
  ;;

  let%expect_test "unit literal" =
    M.init [] [] [ Term TmUnit ] [] |> M.run |> show_value |> print_endline;
    [%expect {| () : Unit |}]
  ;;
end

module Basic = CheckMachine (Stlc.Basic)

(* let%expect_test "eval single literal int" =
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

let%expect_test "eval single literal unit" =
  let instructions = [ Term TmUnit ] in
  let res = run (empty instructions) in
  print_endline (show_value res);
  [%expect {| () : Unit |}]
;;

let%expect_test "eval single literal int with dump" =
  let res =
    run
      { stack = [ Unit ]; env = []; control = []; dump = [ [], [], [ Term (TmInt 0) ] ] }
  in
  print_endline (show_value res);
  [%expect {| 0 : Int |}]
;;

let%expect_test "eval single literal bool with dump" =
  let res =
    run
      { stack = [ Unit ]
      ; env = []
      ; control = []
      ; dump = [ [], [], [ Term (TmBool false) ] ]
      }
  in
  print_endline (show_value res);
  [%expect {| false : Bool |}]
;;

let%expect_test "eval var" =
  let res =
    run { stack = []; env = [ "a", Int 1 ]; control = [ Term (TmVar 0) ]; dump = [] }
  in
  print_endline (show_value res);
  [%expect {| 1 : Int |}]
;;

let%expect_test "eval var 2" =
  let res =
    run
      { stack = []
      ; env = [ "a", Int 1; "b", Bool true ]
      ; control = [ Term (TmVar 1) ]
      ; dump = []
      }
  in
  print_endline (show_value res);
  [%expect {| true : Bool |}]
;;

let%expect_test "eval var with dump" =
  let res =
    run
      { stack = [ Unit ]
      ; env = []
      ; control = []
      ; dump = [ [], [ "a", Int 1 ], [ Term (TmVar 0) ] ]
      }
  in
  print_endline (show_value res);
  [%expect {| 1 : Int |}]
;;

let%expect_test "eval op" =
  let res =
    run { stack = [ Int 1; Int 2 ]; env = []; control = [ Term (TmOp ( + )); Apply]; dump = [] }
  in
  print_endline (show_value res);
  [%expect {| 3 : Int |}]
;; *)
