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

  let%test_unit "bool var" =
    Quickcheck.test Bool.quickcheck_generator ~f:(fun x ->
      [%test_eq: Bool.t Option.t]
        (Some x)
        (extract_bool @@ M.run @@ M.init [] [ "a", Bool x ] [ Term (TmVar 0) ] []))
  ;;

  let%test_unit "int var" =
    Quickcheck.test (Int.gen_incl Int.min_value Int.max_value) ~f:(fun x ->
      [%test_eq: Int.t Option.t]
        (Some x)
        (extract_int @@ M.run @@ M.init [] [ "a", Int x ] [ Term (TmVar 0) ] []))
  ;;

  let%test_unit "add 1" =
    Quickcheck.test
      (Int.gen_incl Int.min_value (Int.max_value - 1))
      ~f:(fun x ->
        [%test_eq: Int.t Option.t]
          (Some (x + 1))
          (extract_int
           @@ M.run
           @@ M.init [ Int 1; Int x ] [] [ Term (TmOp ( + )); Apply ] []))
  ;;

  let%test_unit "sub 1" =
    Quickcheck.test
      (Int.gen_incl (Int.min_value + 1) Int.max_value)
      ~f:(fun x ->
        [%test_eq: Int.t Option.t]
          (Some (x - 1))
          (extract_int
           @@ M.run
           @@ M.init [ Int x; Int 1 ] [] [ Term (TmOp ( - )); Apply ] []))
  ;;

  let%test_unit "add from control" =
    Quickcheck.test
      (Int.gen_incl Int.min_value (Int.max_value - 1))
      ~f:(fun x ->
        [%test_eq: Int.t Option.t]
          (Some (x + 1))
          (extract_int
           @@ M.run
           @@ M.init [] [] [ Term (TmInt x); Term (TmInt 1); Term (TmOp ( + )); Apply ] []
          ))
  ;;

  let%test_unit "sub from control" =
    Quickcheck.test
      (Int.gen_incl (Int.min_value + 1) Int.max_value)
      ~f:(fun x ->
        [%test_eq: Int.t Option.t]
          (Some (x - 1))
          (extract_int
           @@ M.run
           @@ M.init [] [] [ Term (TmInt 1); Term (TmInt x); Term (TmOp ( - )); Apply ] []
          ))
  ;;

  let%expect_test "unit literal" =
    M.init [] [] [ Term TmUnit ] [] |> M.run |> show_value |> print_endline;
    [%expect {| () : Unit |}]
  ;;

  let%expect_test "abstraction" =
    M.init [] [ "a", Int 0; "b", Int 2 ] [ Term (TmAbs ("a", TyInt, TmVar 0)) ] []
    |> M.run
    |> show_value
    |> print_endline;
    [%expect {| {env: [(a = 0 : Int)(b = 2 : Int)], id: a, e: [0/x]} : Closure |}]
  ;;
end

module Basic = CheckMachine (Stlc.Basic)
