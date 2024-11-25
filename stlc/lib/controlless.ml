open Types

type stack = (value * ty) list
type env = (string * value * ty) list

type t =
  { stack : stack
  ; env : env
  }

type final = (stack, err) Result.t

let reset : (unit -> 'a) -> 'a = fun thunk -> thunk ()
let ( >>= ) = Result.bind

let rec eval (t : term) ({ stack = s; env = e } : t) =
  typecheck (context_of_env e) t
  >>= fun typ ->
  match t with
  | TmUnit -> Ok { stack = s; env = e }
  | TmInt x -> Ok { stack = (Int x, typ) :: s; env = e }
  | TmBool x -> Ok { stack = (Bool x, typ) :: s; env = e }
  | TmVar x ->
    let _, vl, ty = List.nth e x in
    Ok { stack = (vl, ty) :: s; env = e }
  | TmAbs (id, ty, body) -> Ok { stack = (Closure (e, id, body), ty) :: s; env = e }
  | TmOp f -> Ok { stack = (Primitive f, typ) :: s; env = e }
  | TmFst ->
    (match s with
     | (Pair (v1, _), TyPair (ty1, _)) :: tl -> Ok { stack = (v1, ty1) :: tl; env = e }
     | _ -> Error (`OperationalError "cannot project without a pair"))
  | TmSnd ->
    (match s with
     | (Pair (_, v2), TyPair (_, ty2)) :: tl -> Ok { stack = (v2, ty2) :: tl; env = e }
     | _ -> Error (`OperationalError "cannot project without a pair"))
  | TmPair ->
    (match s with
     | (v1, ty1) :: (v2, ty2) :: tl ->
       Ok { stack = (Pair (v1, v2), TyPair (ty1, ty2)) :: tl; env = e }
     | _ -> Error (`OperationalError "need two values on the stack to form a pair"))
  | TmApp (t0, t1) ->
    eval t1 { stack = s; env = e }
    >>= fun { stack = s'; env = e' } ->
    eval t0 { stack = s'; env = e' }
    >>= fun { stack = s''; env = e'' } -> apply { stack = s''; env = e'' }

and apply { stack = s; env = e } =
  match s with
  | (Primitive f, _) :: (Int a, TyInt) :: tl ->
    Ok { stack = (Int (f a), TyInt) :: tl; env = e }
  | (Closure (e, id, t), typ) :: (v1, tyv1) :: tl ->
    if typ = tyv1
    then
      reset (fun () -> eval t { stack = []; env = (id, v1, tyv1) :: e })
      >>= fun { stack = s'; env = _ } -> Ok { stack = List.hd s' :: tl; env = e }
    else Error `TypeError
  | [] -> Error (`OperationalError "can't apply when stack is empty")
  | _ -> Error (`OperationalError "invalid operator")
;;

type 'a init =
  [ `Default
  | `Specific of 'a
  ]

let get_or (x : 'a init) y =
  match x with
  | `Specific f -> f
  | `Default -> y
;;

let run ?(stack : stack init = `Default) ?(env : env init = `Default) t =
  let s = get_or stack [] in
  let e = get_or env [] in
  reset (fun () -> eval t { stack = s; env = e })
  >>= fun { stack = s'; env = _ } -> Ok (List.hd s')
;;
