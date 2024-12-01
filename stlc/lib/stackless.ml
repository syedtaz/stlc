open Types

type env = (string * value * ty) list
type final = (value * ty, err) Result.t

type t =
  { value : value * ty
  ; env : env
  }

type cont = t -> (value * ty, err) Result.t

type s =
  { a : value * ty
  ; b : value * ty
  }

let reset : (unit -> 'a) -> 'a = fun thunk -> thunk ()

let rec eval (t : term) (e : env) =
  typecheck (context_of_env e) t
  >>= fun typ ->
  match t with
  | TmUnit -> Ok { value = Unit, typ; env = e }
  | TmInt x -> Ok { value = Int x, typ; env = e }
  | TmBool x -> Ok { value = Bool x, typ; env = e }
  | TmVar x ->
    let _, vl, ty = List.nth e x in
    Ok { value = vl, ty; env = e }
  | TmAbs (id, ty, body) -> Ok { value = Closure (e, id, body), ty; env = e }
  | TmOp f -> Ok { value = Primitive f, typ; env = e }
  | TmApp (t0, t1) ->
    eval t1 e
    >>= fun { value = v1, ty1; env = e' } ->
    eval t0 e'
    >>= fun { value = v0, ty0; env = e'' } -> apply { a = v0, ty0; b = v1, ty1 } e''
  | _ -> failwith "unimplemented"

and apply (s : s) (e : env) =
  match s with
  | { a = Primitive f, _; b = Int x, TyInt } -> Ok { value = Int (f x), TyInt; env = e }
  | { a = Closure (e', id, t), typ; b = v1, tyv1 } ->
    if typ = tyv1
    then
      reset (fun () -> eval t ((id, v1, tyv1) :: e'))
      >>= fun { value = v', ty'; env = _ } -> Ok { value = v', ty'; env = e }
    else Error `TypeError
  | _ -> Error (`OperationalError "invalid operator")
;;

let run ?(cont : cont init = `Default) ?(env : env init = `Default) t =
  let e = get_or env [] in
  let c = get_or cont (fun { value = v; env = _ } -> Ok v) in
  reset (fun () -> eval t e) >>= c
;;
