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

  let incr x = x + 1
  let decr x = x - 1

  (* Given that quickcheck generates an integer x, a machine with the literal
     x on the control should push the value onto the stack and evaluate to
     that value. *)
  let%test_unit "int literal" =
    Quickcheck.test (Int.gen_incl Int.min_value Int.max_value) ~f:(fun x ->
      [%test_eq: Int.t Option.t]
        (Some x)
        (extract_int @@ M.run ~debug:false @@ M.init [] [] [ Term (TmInt x) ] []))
  ;;

  (*  Given that quickcheck generates an boolean x, a machine with the literal
      x on the control should push the value onto the stack and evaluate to
      that value. *)
  let%test_unit "bool literal" =
    Quickcheck.test Bool.quickcheck_generator ~f:(fun x ->
      [%test_eq: Bool.t Option.t]
        (Some x)
        (extract_bool @@ M.run ~debug:false @@ M.init [] [] [ Term (TmBool x) ] []))
  ;;

  (* Given that quickcheck generates an boolean x, a machine with a single
     variable binding to x in the environment and a term to extract the first
     index on the control should retrieve the value x from the environment, push
     it onto the stack and evaluate to that value.*)
  let%test_unit "bool var" =
    Quickcheck.test Bool.quickcheck_generator ~f:(fun x ->
      [%test_eq: Bool.t Option.t]
        (Some x)
        (extract_bool
         @@ M.run ~debug:false
         @@ M.init [] [ "a", Bool x ] [ Term (TmVar 0) ] []))
  ;;

  (* Given that quickcheck generates an integer x, a machine with a single
     variable binding to x in the environment and a term to extract the first
     index on the control should retrieve the value x from the environment, push
     it onto the stack and evaluate to that value.*)
  let%test_unit "int var" =
    Quickcheck.test (Int.gen_incl Int.min_value Int.max_value) ~f:(fun x ->
      [%test_eq: Int.t Option.t]
        (Some x)
        (extract_int
         @@ M.run ~debug:false
         @@ M.init [] [ "a", Int x ] [ Term (TmVar 0) ] []))
  ;;

  (* Given that quickcheck generates an integer x, a machine with x on the stack
     and instructions to increment a number should evaluate to x + 1. *)
  let%test_unit "incr 1" =
    Quickcheck.test
      (Int.gen_incl Int.min_value (Int.max_value - 1))
      ~f:(fun x ->
        [%test_eq: Int.t Option.t]
          (Some (x + 1))
          (extract_int
           @@ M.run ~debug:false
           @@ M.init [ Int x ] [] [ Term (TmOp incr); Apply ] []))
  ;;

  (* Given that quickcheck generates an integer x, a machine with x on the stack
     and instructions to decrement a number should evaluate to x - 1. *)
  let%test_unit "decr 1" =
    Quickcheck.test
      (Int.gen_incl (Int.min_value + 1) Int.max_value)
      ~f:(fun x ->
        [%test_eq: Int.t Option.t]
          (Some (x - 1))
          (extract_int
           @@ M.run ~debug:false
           @@ M.init [ Int x ] [] [ Term (TmOp decr); Apply ] []))
  ;;

  (* Given that quickcheck generates an integer x, a machine with instructions
     that contain an x literal followed by an increment instruction should
     evaluate to x + 1. *)
  let%test_unit "incr from control" =
    Quickcheck.test
      (Int.gen_incl Int.min_value (Int.max_value - 1))
      ~f:(fun x ->
        [%test_eq: Int.t Option.t]
          (Some (x + 1))
          (extract_int
           @@ M.run
           @@ M.init [] [] [ Term (TmInt x); Term (TmOp incr); Apply ] []))
  ;;

  (* Given that quickcheck generates an integer x, a machine with instructions
     that contain an x literal followed by an decrement instruction should
     evaluate to x - 1. *)
  let%test_unit "decr from control" =
    Quickcheck.test
      (Int.gen_incl (Int.min_value + 1) Int.max_value)
      ~f:(fun x ->
        [%test_eq: Int.t Option.t]
          (Some (x - 1))
          (extract_int
           @@ M.run
           @@ M.init [] [] [ Term (TmInt x); Term (TmOp decr); Apply ] []))
  ;;

  (* Given that quickcheck generates an integer x, evaluate (λy.incr y)(x) *)
  let%test_unit "incr app" =
    Quickcheck.test
      (Int.gen_incl Int.min_value (Int.max_value - 1))
      ~f:(fun x ->
        [%test_eq: Int.t Option.t]
          (Some (x + 1))
          (extract_int @@ M.run @@ M.init [] [] [ Term (TmApp (TmOp incr, TmInt x)) ] []))
  ;;

  (* Given that quickcheck generates an integer x, evaluate (λy.decr y)(x) *)
  let%test_unit "decr app" =
    Quickcheck.test
      (Int.gen_incl (Int.min_value + 1) Int.max_value)
      ~f:(fun x ->
        [%test_eq: Int.t Option.t]
          (Some (x - 1))
          (extract_int @@ M.run @@ M.init [] [] [ Term (TmApp (TmOp decr, TmInt x)) ] []))
  ;;

  (* Given that quickcheck generates an integer x, evaluate a machine with
     an empty control, the integer at the top of the stack and (decr x)
     on the dump. The machine must be able to return the value from the stack,
     restore the state from the dump and continue. *)
  let%test_unit "function return" =
    Quickcheck.test
      (Int.gen_incl (Int.min_value + 1) Int.max_value)
      ~f:(fun x ->
        [%test_eq: Int.t Option.t]
          (Some (x - 1))
          (extract_int
           @@ M.run
           @@ M.init [ Int x ] [] [] [ [], [], [ Term (TmOp decr); Apply ] ]))
  ;;

  (* Given that quickcheck generates an integer x, and the dump contains
     (decr x), evaluate (λx.x)(x). This is to test that the machine
     can successfully return from a lambda abstraction, restore from the
     dump and continue. *)
  let%test_unit "abstract and return" =
    Quickcheck.test
      (Int.gen_incl (Int.min_value + 1) Int.max_value)
      ~f:(fun x ->
        [%test_eq: Int.t Option.t]
          (Some (x - 1))
          (extract_int
           @@ M.run
           @@ M.init
                []
                []
                [ Term (TmInt x); Term (TmAbs ("", TyInt, TmVar 0)); Apply ]
                [ [], [], [ Term (TmOp decr); Apply ] ]))
  ;;

  (* Given that quickcheck generates an integer x, evaluate λf.(f x)(decr). This
     is to test that the machine can read from the environment in the closure
     and return. *)
  let%test_unit "abstract and apply" =
    Quickcheck.test
      (Int.gen_incl (Int.min_value + 1) Int.max_value)
      ~f:(fun x ->
        [%test_eq: Int.t Option.t]
          (Some (x - 1))
          (extract_int
           @@ M.run
           @@ M.init
                []
                []
                [ Term (TmOp decr)
                ; Term (TmAbs ("f", TyArr (TyInt, TyInt), TmApp (TmVar 0, TmInt x)))
                ; Apply
                ]
                []))
  ;;

  (* Given that quickcheck generates an integer x, evaluate λf.(λx.(f x))(x)(decr) *)
  let%test_unit "abstract, abstract and apply" =
    Quickcheck.test
      (Int.gen_incl (Int.min_value + 1) Int.max_value)
      ~f:(fun x ->
        [%test_eq: Int.t Option.t]
          (Some (x - 1))
          (extract_int
           @@ M.run
           @@ M.init
                []
                []
                [ Term (TmInt x)
                ; Term (TmOp decr)
                ; Term
                    (TmAbs
                       ( "f"
                       , TyArr (TyInt, TyInt)
                       , TmAbs ("x", TyInt, TmApp (TmVar 1, TmVar 0)) ))
                ; Apply
                ; Apply
                ]
                []))
  ;;
end

module Basic = CheckMachine (Stlc.Basic)
