(** Splits the basic machine into two mutually recursive evaluators. *)

open Types
include Machine_intf

type t =
  { stack : stack
  ; env : env
  ; control : ctl
  ; dump : dump
  }

let init s e c d = { stack = s; env = e; control = c; dump = d }

let show (m : t) =
  let { stack; env; control; dump } = m in
  let conv f lst = String.concat "," @@ List.map f lst in
  let ps x = conv (fun (x, _) -> show_value x) x in
  let pe x = conv (fun (x, y, _) -> Format.sprintf "%s = %s" x (show_value y)) x in
  let pc x = conv show_control x in
  let pd x = conv (fun (s, e, c) -> Format.sprintf "%s;%s;%s" (ps s) (pe e) (pc c)) x in
  Format.sprintf "(%s;%s;%s;%s)" (ps stack) (pe env) (pc control) (pd dump)
;;

let ( >>= ) = Result.bind

let rec run_c ~debug m =
  let { stack; env; control; dump } = m in
  let () = if debug then Format.print_string (show m) else () in
  match control with
  | [] -> run_d ~debug m
  | Term t :: c -> run_t ~debug t { stack; env; control = c; dump }
  | Apply :: c -> run_a ~debug { stack; env; control = c; dump }

and run_d ~debug m =
  let { stack; dump; _ } = m in
  let () = if debug then Format.print_string (show m) else () in
  match dump, stack with
  | [], [] -> Ok (Unit, TyUnit)
  | [], (h, typ) :: _ -> Ok (h, typ)
  | (s, e, c) :: d, [] -> run_c ~debug { stack = s; env = e; control = c; dump = d }
  | (s, e, c) :: d, h :: _ ->
    run_c ~debug { stack = h :: s; env = e; control = c; dump = d }

and run_t ~debug term m =
  let { stack; env; control; dump } = m in
  let () = if debug then Format.print_string (show m) else () in
  typecheck (context_of_env env) term
  >>= fun typ ->
  match term with
  | TmUnit -> run_c ~debug { stack; env; control; dump }
  | TmFst ->
    (match stack with
     | (Pair (v1, _), TyPair (ty1, _)) :: tl ->
       run_c ~debug { stack = (v1, ty1) :: tl; env; control; dump }
     | _ -> Error (`OperationalError "cannot project without a pair"))
  | TmSnd ->
    (match stack with
     | (Pair (_, v2), TyPair (_, ty2)) :: tl ->
       run_c ~debug { stack = (v2, ty2) :: tl; env; control; dump }
     | _ -> Error (`OperationalError "cannot project without a pair"))
  | TmPair ->
    (match stack with
     | (v1, ty1) :: (v2, ty2) :: tl ->
       run_c
         ~debug
         { stack = (Pair (v1, v2), TyPair (ty1, ty2)) :: tl; env; control; dump }
     | _ -> Error (`OperationalError "need two values on the stack to form a pair"))
  | TmInt x -> run_c ~debug { stack = (Int x, typ) :: stack; env; control; dump }
  | TmBool x -> run_c ~debug { stack = (Bool x, typ) :: stack; env; control; dump }
  | TmVar x ->
    let _, vl, ty = List.nth env x in
    run_c ~debug { stack = (vl, ty) :: stack; env; control; dump }
  | TmAbs (id, ty, body) ->
    run_c ~debug { stack = (Closure (env, id, body), ty) :: stack; env; control; dump }
  | TmApp (t1, t2) ->
    run_t ~debug t2 { stack; env; control = Term t1 :: Apply :: control; dump }
  | TmOp f -> run_c ~debug { stack = (Primitive f, typ) :: stack; env; control; dump }

and run_a ~debug m =
  let { stack; env; control; dump } = m in
  let () = if debug then Format.print_string (show m) else () in
  match stack with
  | [] -> Error (`OperationalError "can't apply when stack is empty")
  | (Closure (e, id, t), typ) :: (v1, tyv1) :: tl ->
    if typ = tyv1
    then
      run_t
        ~debug
        t
        { stack = []
        ; env = (id, v1, tyv1) :: e
        ; control = []
        ; dump = (tl, env, control) :: dump
        }
    else Error `TypeError
  | (Primitive f, _) :: (Int a, TyInt) :: tl ->
    run_c ~debug { stack = (Int (f a), TyInt) :: tl; env; control; dump }
  | _ -> Error (`OperationalError "invalid operator")
;;

let run ?(debug=false) m = run_c ~debug m
