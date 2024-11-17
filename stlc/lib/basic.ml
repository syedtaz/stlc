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

let rec run (m : t) =
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
    run { stack = top :: s'; env = e'; control = ctl'; dump = tl }
  | { stack = s; env = e; control = ctl_hd :: ctl_tl; dump = d } ->
    (match ctl_hd with
     | Term t ->
       (match t with
        (* If the control contains a literal term, convert it into a value and
           push it onto the stack. *)
        | TmInt i -> run { stack = Int i :: s; env = e; control = ctl_tl; dump = d }
        | TmBool b -> run { stack = Bool b :: s; env = e; control = ctl_tl; dump = d }
        | TmUnit -> run { stack = s; env = e; control = ctl_tl; dump = d }
        (* If the control contains a var term, find the value of its binding and
           push it onto the stack. *)
        | TmVar i ->
          let _, vl = List.nth e i in
          run { stack = vl :: s; env = e; control = ctl_tl; dump = d }
        (* If the control contains an application term, add each individual term
           followed by an apply tag onto the control. *)
        | TmApp (t1, t2) ->
          run
            { stack = s
            ; env = e
            ; control = Term t2 :: Term t1 :: Apply :: ctl_tl
            ; dump = d
            }
        (* If the control contains an abstraction, capture the current environment
           and construct a closure with the body of the abstraction and push it onto the stack. *)
        | TmAbs (id, _, body) ->
          run { stack = Closure (e, id, body) :: s; env = e; control = ctl_tl; dump = d }
        (* If the control contains a builtin operation literal, convert it into a
           value and push it onto the stack. *)
        | TmOp f -> run { stack = Binop f :: s; env = e; control = ctl_tl; dump = d })
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
           | Binop f ->
             (match s' with
              | v1 :: v2 :: tl ->
                (match v1, v2 with
                 | Int a, Int b ->
                   run { stack = Int (f a b) :: tl; env = e; control = ctl_tl; dump = d }
                 | _ -> failwith "invalid operands")
              | _ -> failwith "invalid operands")
           | Closure (e, id, t) ->
             (match s' with
              | v1 :: tl ->
                run
                  { stack = []
                  ; env = (id, v1) :: e
                  ; control = [ Term t ]
                  ; dump = (tl, e, ctl_tl) :: d
                  }
              | _ -> failwith "invalid closure"))))

