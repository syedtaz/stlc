open Types

type stack = (value * ty) list
type env = (string * value * ty) list

type state =
  { stack : stack
  ; env : env
  }

type ctl = state -> (stack, err) Result.t

type t =
  { stack : stack
  ; env : env
  ; control : ctl
  }

let ( >>= ) = Result.bind

let rec eval (t : term) ({ stack = s; env = e; control = c } : t) =
  typecheck (context_of_env e) t
  >>= fun typ ->
  match t with
  | TmUnit -> c { stack = s; env = e }
  | TmInt x -> c { stack = (Int x, typ) :: s; env = e }
  | TmBool x -> c { stack = (Bool x, typ) :: s; env = e }
  | TmVar x ->
    let _, vl, ty = List.nth e x in
    c { stack = (vl, ty) :: s; env = e }
  | TmAbs (id, ty, body) -> c { stack = (Closure (e, id, body), ty) :: s; env = e }
  | TmApp (t0, t1) ->
    eval
      t1
      { stack = s
      ; env = e
      ; control =
          (fun { stack = s'; env = e' } ->
            eval
              t0
              { stack = s'
              ; env = e'
              ; control =
                  (fun { stack = s''; env = e'' } ->
                    apply { stack = s''; env = e''; control = c })
              })
      }
  | TmOp f -> c { stack = (Primitive f, typ) :: s; env = e }
  | TmFst ->
    (match s with
     | (Pair (v1, _), TyPair (ty1, _)) :: tl -> c { stack = (v1, ty1) :: tl; env = e }
     | _ -> Error (`OperationalError "cannot project without a pair"))
  | TmSnd ->
    (match s with
     | (Pair (_, v2), TyPair (_, ty2)) :: tl -> c { stack = (v2, ty2) :: tl; env = e }
     | _ -> Error (`OperationalError "cannot project without a pair"))
  | TmPair ->
    (match s with
     | (v1, ty1) :: (v2, ty2) :: tl ->
       c { stack = (Pair (v1, v2), TyPair (ty1, ty2)) :: tl; env = e }
     | _ -> Error (`OperationalError "need two values on the stack to form a pair"))

and apply ({ stack = s; env = e; control = c } : t) =
  match s with
  | (Primitive f, _) :: (Int a, TyInt) :: tl ->
    c { stack = (Int (f a), TyInt) :: tl; env = e }
  | (Closure (e, id, t), typ) :: (v1, tyv1) :: tl ->
    if typ = tyv1
    then
      eval
        t
        { stack = []
        ; env = (id, v1, tyv1) :: e
        ; control = (fun { stack = s'; _ } -> Ok s')
        }
      >>= fun res ->
      let v = List.hd res in
      c { stack = v :: tl; env = e }
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

let run
  ?(stack : stack init = `Default)
  ?(env : env init = `Default)
  ?(control : ctl init = `Default)
  t
  =
  let c = get_or control (fun { stack = s'; env = _ } -> Ok s') in
  let s = get_or stack [] in
  let e = get_or env [] in
  eval t { stack = s; env = e; control = c } >>= fun res -> Ok (List.hd res)
;;
