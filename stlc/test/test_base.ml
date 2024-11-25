open Helpers
open Core

(* Given that quickcheck generates an integer x, a machine with the literal
   x on the control should push the value onto the stack and evaluate to
   that value. *)
let int_literal y =
  Quickcheck.test (Int.gen_incl Int.min_value Int.max_value) ~f:(fun x ->
    [%test_eq: Int.t Option.t] (Some x) (extract_int (y x)))
;;

(*  Given that quickcheck generates an boolean x, a machine with the literal
    x on the control should push the value onto the stack and evaluate to
    that value. *)
let bool_literal y =
  Quickcheck.test Bool.quickcheck_generator ~f:(fun x ->
    [%test_eq: Bool.t Option.t] (Some x) (extract_bool (y x)))
;;

(* Given that quickcheck generates an boolean x, a machine with a single
   variable binding to x in the environment and a term to extract the first
   index on the control should retrieve the value x from the environment, push
   it onto the stack and evaluate to that value.*)
let bool_var y =
  Quickcheck.test Bool.quickcheck_generator ~f:(fun x ->
    [%test_eq: Bool.t Option.t] (Some x) (extract_bool (y x)))
;;

(* Given that quickcheck generates an integer x, a machine with a single
   variable binding to x in the environment and a term to extract the first
   index on the control should retrieve the value x from the environment, push
   it onto the stack and evaluate to that value.*)
let int_var y =
  Quickcheck.test (Int.gen_incl Int.min_value Int.max_value) ~f:(fun x ->
    [%test_eq: Int.t Option.t] (Some x) (extract_int (y x)))
;;

(* Given that quickcheck generates an integer x, a machine with x on the stack
   and instructions to increment a number should evaluate to x + 1. *)
let incr_1 y =
  Quickcheck.test
    (Int.gen_incl Int.min_value (Int.max_value - 1))
    ~f:(fun x -> [%test_eq: Int.t Option.t] (Some (x + 1)) (extract_int (y x)))
;;

(* Given that quickcheck generates an integer x, a machine with x on the stack
   and instructions to decrement a number should evaluate to x - 1. *)
let decr_1 y =
  Quickcheck.test
    (Int.gen_incl (Int.min_value + 1) Int.max_value)
    ~f:(fun x -> [%test_eq: Int.t Option.t] (Some (x - 1)) (extract_int (y x)))
;;

(* Given that quickcheck generates an integer x, a machine with instructions
   that contain an x literal followed by an increment instruction should
   evaluate to x + 1. *)
let incr_from_control y =
  Quickcheck.test
    (Int.gen_incl Int.min_value (Int.max_value - 1))
    ~f:(fun x -> [%test_eq: Int.t Option.t] (Some (x + 1)) (extract_int (y x)))
;;

(* Given that quickcheck generates an integer x, a machine with instructions
   that contain an x literal followed by an decrement instruction should
   evaluate to x - 1. *)
let decr_from_control y =
  Quickcheck.test
    (Int.gen_incl (Int.min_value + 1) Int.max_value)
    ~f:(fun x -> [%test_eq: Int.t Option.t] (Some (x - 1)) (extract_int (y x)))
;;

(* Given that quickcheck generates an integer x, evaluate (λy.incr y)(x) *)
let incr_app y =
  Quickcheck.test
    (Int.gen_incl Int.min_value (Int.max_value - 1))
    ~f:(fun x -> [%test_eq: Int.t Option.t] (Some (x + 1)) (extract_int (y x)))
;;

(* Given that quickcheck generates an integer x, evaluate (λy.decr y)(x) *)
let decr_app y =
  Quickcheck.test
    (Int.gen_incl (Int.min_value + 1) Int.max_value)
    ~f:(fun x -> [%test_eq: Int.t Option.t] (Some (x - 1)) (extract_int (y x)))
;;

(* Given that quickcheck generates an integer x, evaluate a machine with
   an empty control, the integer at the top of the stack and (decr x)
   on the dump. The machine must be able to return the value from the stack,
   restore the state from the dump and continue. *)
let function_return y =
  Quickcheck.test
    (Int.gen_incl (Int.min_value + 1) Int.max_value)
    ~f:(fun x -> [%test_eq: Int.t Option.t] (Some (x - 1)) (extract_int (y x)))
;;

(* Given that quickcheck generates an integer x, and the dump contains
   (decr x), evaluate (λx.x)(x). This is to test that the machine
   can successfully return from a lambda abstraction, restore from the
   dump and continue. *)
let abstract_and_return y =
  Quickcheck.test
    (Int.gen_incl (Int.min_value + 1) Int.max_value)
    ~f:(fun x -> [%test_eq: Int.t Option.t] (Some (x - 1)) (extract_int (y x)))
;;

(* Given that quickcheck generates an integer x, evaluate λf.(f x)(decr). This
   is to test that the machine can read from the environment in the closure
   and return. *)
let abstract_and_apply y =
  Quickcheck.test
    (Int.gen_incl (Int.min_value + 1) Int.max_value)
    ~f:(fun x -> [%test_eq: Int.t Option.t] (Some (x - 1)) (extract_int (y x)))
;;

(* Given that quickcheck generates an boolean x, try to evaluate λf.(f x)(decr).
   This should not type check. *)
let abstract_and_apply_err y =
  Quickcheck.test Bool.quickcheck_generator ~f:(fun x ->
    [%test_eq: String.t Option.t] (Some "typerror") (must_be_error (y x)))
;;

(* Given that quickcheck generates an integer x, evaluate λf.(λx.(f x))(x)(decr).
   This is to test that the machine can evaluate multiple, nested abstractions. *)
let abstract_abstract_and_apply y =
  Quickcheck.test
    (Int.gen_incl (Int.min_value + 1) Int.max_value)
    ~f:(fun x -> [%test_eq: Int.t Option.t] (Some (x - 1)) (extract_int (y x)))
;;

(* Given that quickcheck generates an integer x, evaluate {x, x}.1. This is to
    test that the machine can project the first element of a pair. *)
let project_1 y =
  Quickcheck.test
    (Int.gen_incl Int.min_value (Int.max_value - 1))
    ~f:(fun x -> [%test_eq: Int.t Option.t] (Some (x + 1)) (extract_int (y x)))
;;

(* Given that quickcheck generates an integer x, evaluate {x, x}.2 This is to
    test that the machine can project the second element of a pair. *)
let project_2 y =
  Quickcheck.test
    (Int.gen_incl Int.min_value (Int.max_value - 1))
    ~f:(fun x -> [%test_eq: Int.t Option.t] (Some (x + 1)) (extract_int (y x)))
;;

(* Given that quickcheck generates an integer x, test that the machine can construct a pair. *)
let construct_pair y =
  Quickcheck.test
    (Int.gen_incl Int.min_value (Int.max_value - 1))
    ~f:(fun x -> [%test_eq: Int.t Option.t] (Some x) (extract_int (y x)))
;;
