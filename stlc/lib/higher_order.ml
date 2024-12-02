open Types

type final = (value * ty, err) Result.t
type stack = (value * ty) list
type env = (string * value * ty) list
type dump = stack -> final

type state =
  { stack : stack
  ; env : env
  ; dump : dump
  ; tail : bool
  }

type ctl = state -> final

type t =
  { stack : stack
  ; env : env
  ; ctl : ctl
  ; dump : dump
  ; tail : bool
  }

let init s e c d t = { stack = s; env = e; ctl = c; dump = d; tail = t }

let rec run_t : term -> t -> final =
  fun (t : term) { stack = s; env = e; ctl = c; dump = d; tail } ->
  typecheck (context_of_env e) t
  >>= fun typ ->
  match t with
  | TmUnit -> c { stack = s; env = e; dump = d; tail }
  | TmInt x -> c { stack = (Int x, typ) :: s; env = e; dump = d; tail }
  | TmBool x -> c { stack = (Bool x, typ) :: s; env = e; dump = d; tail }
  | TmVar x ->
    let _, vl, ty = List.nth e x in
    c { stack = (vl, ty) :: s; env = e; dump = d; tail }
  | TmAbs (id, ty, body) ->
    c { stack = (Closure (e, id, body), ty) :: s; env = e; dump = d; tail }
  | TmApp (t0, t1) ->
    run_t
      t1
      { stack = s
      ; env = e
      ; ctl =
          (fun { stack = s'; env = e'; dump = d'; tail = tail' } ->
            run_t
              t0
              { stack = s'
              ; env = e'
              ; ctl =
                  (fun { stack = s''; env = e''; dump = d''; tail = tail'' } ->
                    run_a { stack = s''; env = e''; ctl = c; dump = d''; tail = tail'' })
              ; dump = d'
              ; tail = tail'
              })
      ; dump = d
      ; tail
      }
  | TmOp f -> c { stack = (Primitive f, typ) :: s; env = e; dump = d; tail }
  | TmFst ->
    (match s with
     | (Pair (v1, _), TyPair (ty1, _)) :: tl ->
       c { stack = (v1, ty1) :: tl; env = e; dump = d; tail }
     | _ -> Error (`OperationalError "cannot project without a pair"))
  | TmSnd ->
    (match s with
     | (Pair (_, v2), TyPair (_, ty2)) :: tl ->
       c { stack = (v2, ty2) :: tl; env = e; dump = d; tail }
     | _ -> Error (`OperationalError "cannot project without a pair"))
  | TmPair ->
    (match s with
     | (v1, ty1) :: (v2, ty2) :: tl ->
       c { stack = (Pair (v1, v2), TyPair (ty1, ty2)) :: tl; env = e; dump = d; tail }
     | _ -> Error (`OperationalError "need two values on the stack to form a pair"))

and run_a : t -> final =
  fun ({ stack = s; env = e; ctl = c; dump = d; tail } : t) ->
  match s with
  | (Primitive f, _) :: (Int a, TyInt) :: tl ->
    c { stack = (Int (f a), TyInt) :: tl; env = e; dump = d; tail }
  | (Closure (e, id, t), typ) :: (v1, tyv1) :: tl ->
    if typ = tyv1
    then (
      match tail with
      | false ->
        run_t
          t
          { stack = []
          ; env = (id, v1, tyv1) :: e
          ; ctl = (fun { stack = s'; env = _; dump = d'; tail = _ } -> d' s')
          ; dump =
              (fun s ->
                let v = List.hd s in
                c { stack = v :: tl; env = e; dump = d; tail })
          ; tail
          }
      | true ->
        run_t
          t
          { stack = []
          ; env = (id, v1, tyv1) :: e
          ; ctl = (fun { stack = s'; env = _; dump = d'; tail = _ } -> d' s')
          ; dump = d
          ; tail
          })
    else Error `TypeError
  | [] -> Error (`OperationalError "can't apply when stack is empty")
  | _ -> Error (`OperationalError "invalid operator")
;;

let run
  ?(stack : stack init = `Default)
  ?(env : env init = `Default)
  ?(control : ctl init = `Default)
  ?(dump : dump init = `Default)
  t
  =
  let s = get_or stack [] in
  let e = get_or env [] in
  let c = get_or control (fun { stack = s; env = _; dump = d; tail = _ } -> d s) in
  let d =
    get_or dump (fun s ->
      match s with
      | [] -> Ok (Unit, TyUnit)
      | (h, typ) :: _ -> Ok (h, typ))
  in
  run_t t (init s e c d false)
;;
