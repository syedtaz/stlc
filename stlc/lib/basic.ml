(** Basic implemention of the SECD machine. Assoc lists are used for the
    environment. *)

open Types
include Machine_intf

type t =
  { stack : (value * ty) list
  ; env : (string * value * ty) list
  ; control : control list
  ; dump : ((value * ty) list * (string * value * ty) list * control list) list
  }

let init s e c d = { stack = s; env = e; control = c; dump = d }

let show (m : t) =
  let stack = List.fold_left (fun acc (x, _) -> acc ^ ";" ^ show_value x) "" m.stack in
  let env =
    List.fold_left
      (fun acc (id, x, _) -> acc ^ ";" ^ Format.sprintf "%s = %s" id (show_value x))
      ""
      m.env
  in
  let control = List.fold_left (fun acc x -> acc ^ ";" ^ show_control x) "" m.control in
  Format.sprintf "(Stack = %s, Env = %s, Control = %s)\n" stack env control
;;

let ( >>= ) = Result.bind

let rec run ?(debug = false) (m : t) =
  let () = if debug then Format.print_string (show m) else () in
  match m with
  (* If control and dump is empty, then the result of the evaluation is on the
     stack. *)
  | { stack = s; env = _; control = []; dump = [] } ->
    (match s with
     | hd :: _ -> Ok hd
     | [] -> Ok (Unit, TyUnit))
  (* If the control is empty but the dump is not empty, take the result of the
     evaluation from the current stack and restore the head of the dump.*)
  | { stack = s; env = _; control = []; dump = hd :: tl } ->
    let top = List.hd s in
    let s', e', ctl' = hd in
    run ~debug { stack = top :: s'; env = e'; control = ctl'; dump = tl }
  | { stack = s; env = e; control = ctl_hd :: ctl_tl; dump = d } ->
    (match ctl_hd with
     | Term t ->
       typecheck (context_of_env e) t
       >>= fun typ ->
       (match t with
        (* If the control contains a literal term, convert it into a value and
           push it onto the stack. *)
        | TmInt i ->
          run ~debug { stack = (Int i, typ) :: s; env = e; control = ctl_tl; dump = d }
        | TmBool b ->
          run ~debug { stack = (Bool b, typ) :: s; env = e; control = ctl_tl; dump = d }
        | TmUnit -> run ~debug { stack = s; env = e; control = ctl_tl; dump = d }
        (* If the control contains a var term, find the value of its binding and
           push it onto the stack. *)
        | TmVar i ->
          let _, vl, ty = List.nth e i in
          run ~debug { stack = (vl, ty) :: s; env = e; control = ctl_tl; dump = d }
        (* If the control contains a projection, look into the stack; extract
           the value of the pair if it exists and push it back. *)
        | TmFst ->
          (match s with
           | (Pair (v1, _), TyPair (ty1, _)) :: tl ->
             run ~debug { stack = (v1, ty1) :: tl; env = e; control = ctl_tl; dump = d }
           | _ -> Error (`OperationalError "cannot project without a pair"))
        | TmSnd ->
          (match s with
           | (Pair (_, v2), TyPair (_, ty2)) :: tl ->
             run ~debug { stack = (v2, ty2) :: tl; env = e; control = ctl_tl; dump = d }
           | _ -> Error (`OperationalError "cannot project without a pair"))
        (* If the control contains a pairing, extract the top two values on the stack
           and construct a pair value. *)
        | TmPair ->
          (match s with
           | (v1, ty1) :: (v2, ty2) :: tl ->
             run
               ~debug
               { stack = (Pair (v1, v2), TyPair (ty1, ty2)) :: tl
               ; env = e
               ; control = ctl_tl
               ; dump = d
               }
           | _ -> Error (`OperationalError "need two values on the stack to form a pair"))
        (* If the control contains an application term, add each individual term
           followed by an apply tag onto the control. *)
        | TmApp (t1, t2) ->
          run
            ~debug
            { stack = s
            ; env = e
            ; control = Term t2 :: Term t1 :: Apply :: ctl_tl
            ; dump = d
            }
        (* If the control contains an abstraction, capture the current environment
           and construct a closure with the body of the abstraction and push it onto the stack. *)
        | TmAbs (id, ty, body) ->
          run
            ~debug
            { stack = (Closure (e, id, body), ty) :: s
            ; env = e
            ; control = ctl_tl
            ; dump = d
            }
        (* If the control contains a builtin operation literal, convert it into a
           value and push it onto the stack. *)
        | TmOp f ->
          run
            ~debug
            { stack = (Primitive f, typ) :: s; env = e; control = ctl_tl; dump = d })
     | Apply ->
       (match s with
        | [] -> Error (`OperationalError "can't apply when stack is empty")
        | (op, ty) :: s' ->
          (match op with
           (* We cannot apply an a value literal. *)
           | Int _ | Unit | Bool _ | Pair _ ->
             Error (`OperationalError "invalid operator")
           (* If the stack contains a builtin operator, then pop two further
              values from the stack; apply the operator and push the result back
              onto the stack. *)
           | Primitive f ->
             (match s' with
              | (v1, tyv1) :: tl ->
                (match v1, tyv1 with
                 | Int a, TyInt ->
                   run
                     ~debug
                     { stack = (Int (f a), TyInt) :: tl
                     ; env = e
                     ; control = ctl_tl
                     ; dump = d
                     }
                 | _ -> Error (`OperationalError "invalid operands"))
              | _ -> Error (`OperationalError "invalid operands"))
           (* If the stack contains a closure followed by a value, then pop the
              stack twice. Take a copy of the stack, environment and control and
              save into into the dump. Then allocate a fresh empty stack, extend
              the environment with the value from the stack bound to the identifier
              in the closure and replace the control with the instructions in
              the body of the closure. *)
           | Closure (e, id, t) ->
             (match s', ty with
              | (v1, tyv1) :: tl, ty when tyv1 = ty ->
                run
                  ~debug
                  { stack = []
                  ; env = (id, v1, tyv1) :: e
                  ; control = [ Term t ]
                  ; dump = (tl, e, ctl_tl) :: d (* TODO! Are we saving the right environment? *)
                  }
              | _ -> Error (`OperationalError "invalid closure")))))
;;
