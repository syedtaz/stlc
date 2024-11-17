(** Basic implemention of the SECD machine. Assoc lists are used for the
    environment. *)

open Types
include Machine_intf

type t =
  { stack : value list
  ; env : (string * value) list
  ; control : control list
  ; dump : (value list * (string * value) list * control list) list
  }

let init s e c d = { stack = s; env = e; control = c; dump = d }

let show (m : t) =
  let stack = List.fold_left (fun acc x -> acc ^ ";" ^ show_value x) "" m.stack in
  let env =
    List.fold_left
      (fun acc (id, x) -> acc ^ ";" ^ Format.sprintf "%s = %s" id (show_value x))
      ""
      m.env
  in
  let control = List.fold_left (fun acc x -> acc ^ ";" ^ show_control x) "" m.control in
  Format.sprintf "(Stack = %s, Env = %s, Control = %s)\n" stack env control
;;

let rec run ?(debug = false) (m : t) =
  let () = if debug then Format.print_string (show m) else () in
  match m with
  (* If control and dump is empty, then the result of the evaluation is on the
     stack. *)
  | { stack = s; env = _; control = []; dump = [] } ->
    (* TODO! Is it guaranteed that there will only be one value on the stack? *)
    (match s with
     | hd :: _ -> hd
     | [] -> Unit)
  (* If the control is empty but the dump is not empty, take the result of the
     evaluation from the current stack and restore the head of the dump.*)
  | { stack = s; env = _; control = []; dump = hd :: tl } ->
    let top = List.hd s in
    let s', e', ctl' = hd in
    run ~debug { stack = top :: s'; env = e'; control = ctl'; dump = tl }
  | { stack = s; env = e; control = ctl_hd :: ctl_tl; dump = d } ->
    (match ctl_hd with
     | Term t ->
       (match t with
        (* If the control contains a literal term, convert it into a value and
           push it onto the stack. *)
        | TmInt i -> run ~debug { stack = Int i :: s; env = e; control = ctl_tl; dump = d }
        | TmBool b -> run ~debug { stack = Bool b :: s; env = e; control = ctl_tl; dump = d }
        | TmUnit -> run ~debug { stack = s; env = e; control = ctl_tl; dump = d }
        (* If the control contains a var term, find the value of its binding and
           push it onto the stack. *)
        | TmVar i ->
          let _, vl = List.nth e i in
          run ~debug { stack = vl :: s; env = e; control = ctl_tl; dump = d }
        (* If the control contains an application term, add each individual term
           followed by an apply tag onto the control. *)
        | TmApp (t1, t2) ->
          run ~debug
            { stack = s
            ; env = e
            ; control = Term t2 :: Term t1 :: Apply :: ctl_tl
            ; dump = d
            }
        (* If the control contains an abstraction, capture the current environment
           and construct a closure with the body of the abstraction and push it onto the stack. *)
        | TmAbs (id, _, body) ->
          run ~debug { stack = Closure (e, id, body) :: s; env = e; control = ctl_tl; dump = d }
        (* If the control contains a builtin operation literal, convert it into a
           value and push it onto the stack. *)
        | TmOp f -> run ~debug { stack = Primitive f :: s; env = e; control = ctl_tl; dump = d })
     | Apply ->
       (match s with
        | [] -> failwith "can't apply when stack is empty"
        | op :: s' ->
          (match op with
           (* We cannot apply an a value literal. *)
           | Int _ | Unit | Bool _ -> failwith "invalid operator"
           (* If the stack contains a builtin operator, then pop two further
              values from the stack; apply the operator and push the result back
              onto the stack. *)
           | Primitive f ->
             (match s' with
              | v1 :: tl ->
                (match v1 with
                 | Int a ->
                   run ~debug { stack = Int (f a) :: tl; env = e; control = ctl_tl; dump = d }
                 | _ -> failwith "invalid operands")
              | _ -> failwith "invalid operands")
           | Closure (e, id, t) ->
             (match s' with
              | v1 :: tl ->
                run ~debug
                  { stack = []
                  ; env = (id, v1) :: e
                  ; control = [ Term t ]
                  ; dump = (tl, e, ctl_tl) :: d
                  }
              | _ -> failwith "invalid closure"))))
;;
