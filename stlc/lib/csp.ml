open Types
include Machine_csp_intf

type t =
  { stack : stack
  ; env : env
  ; control : ctl
  ; dump : dump
  }

let init ?(c = None) ?(d = None) s e =
  { stack = s
  ; env = e
  ; control =
      (match c with
       | None -> fun (s, _, d) -> d s
       | Some f -> f)
  ; dump =
      (match d with
       | None ->
         fun lst ->
           (match lst with
            | [] -> Ok (Unit, TyUnit)
            | (h, typ) :: _ -> Ok (h, typ))
       | Some f -> f)
  }
;;

let show (m : t) =
  let { stack; env; _ } = m in
  let conv f lst = String.concat "," @@ List.map f lst in
  let ps x = conv (fun (x, _) -> show_value x) x in
  let pe x = conv (fun (x, y, _) -> Format.sprintf "%s = %s" x (show_value y)) x in
  Format.sprintf "(%s;%s)\n" (ps stack) (pe env)
;;

let ( >>= ) = Result.bind

let rec run_t ~debug term m =
  let { stack; env; control; dump } = m in
  let () = if debug then Format.print_string (show m) else () in
  typecheck (context_of_env env) term
  >>= fun typ ->
  match term with
  | TmUnit -> control (stack, env, dump)
  | TmFst ->
    (match stack with
     | (Pair (v1, _), TyPair (ty1, _)) :: tl -> control ((v1, ty1) :: tl, env, dump)
     | _ -> Error (`OperationalError "cannot project without a pair"))
  | TmSnd ->
    (match stack with
     | (Pair (_, v2), TyPair (_, ty2)) :: tl -> control ((v2, ty2) :: tl, env, dump)
     | _ -> Error (`OperationalError "cannot project without a pair"))
  | TmPair ->
    (match stack with
     | (v1, ty1) :: (v2, ty2) :: tl ->
       control ((Pair (v1, v2), TyPair (ty1, ty2)) :: tl, env, dump)
     | _ -> Error (`OperationalError "need two values on the stack to form a pair"))
  | TmInt x -> control ((Int x, typ) :: stack, env, dump)
  | TmBool x -> control ((Bool x, typ) :: stack, env, dump)
  | TmVar x ->
    let _, vl, ty = List.nth env x in
    control ((vl, ty) :: stack, env, dump)
  | TmAbs (id, ty, body) -> control ((Closure (env, id, body), ty) :: stack, env, dump)
  | TmApp (t1, t2) ->
    run_t
      ~debug
      t2
      { stack
      ; env
      ; control =
          (fun (s, e, d) ->
            run_t
              ~debug
              t1
              { stack = s
              ; env = e
              ; control =
                  (fun (s, e, d) ->
                    run_a ~debug { stack = s; env = e; control; dump = d })
              ; dump = d
              })
      ; dump
      }
  | TmOp f -> control ((Primitive f, typ) :: stack, env, dump)
(* | TmOp f -> run_a ~debug { stack = (Primitive f, typ) :: stack; env; control; dump } *)

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
        ; control = (fun (s, _, d) -> d s)
        ; dump =
            (fun lst ->
              match lst with
              | [] -> control (stack, env, dump)
              | v :: _ -> control (v :: tl, env, dump))
        }
    else Error `TypeError
  | (Primitive f, _) :: (Int a, TyInt) :: tl ->
    control ((Int (f a), TyInt) :: tl, env, dump)
  | _ -> Error (`OperationalError "invalid operator")
;;

let run ?(debug = false) t m = run_t ~debug t m
