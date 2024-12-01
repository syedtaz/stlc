open Types

type final = (value * ty, err) Result.t
type stack = (value * ty) list
type env = (string * value * ty) list
type dump = stack -> final

type state =
  { stack : stack
  ; env : env
  ; dump : dump
  }

type ctl = state -> final

type t =
  { stack : stack
  ; env : env
  ; ctl : ctl
  ; dump : dump
  }

let init s e c d = { stack = s; env = e; ctl = c; dump = d }

let rec run_t : term -> t -> final =
  fun (t : term) { stack = s; env = e; ctl = c; dump = d } ->
  typecheck (context_of_env e) t
  >>= fun typ ->
  match t with
  | TmUnit -> c { stack = s; env = e; dump = d }
  | TmInt x -> c { stack = (Int x, typ) :: s; env = e; dump = d }
  | TmBool x -> c { stack = (Bool x, typ) :: s; env = e; dump = d }
  | TmVar x ->
    let _, vl, ty = List.nth e x in
    c { stack = (vl, ty) :: s; env = e; dump = d }
  | TmAbs (id, ty, body) ->
    c { stack = (Closure (e, id, body), ty) :: s; env = e; dump = d }
  | TmApp (t0, t1) ->
    run_t
      t1
      { stack = s
      ; env = e
      ; ctl =
          (fun { stack = s'; env = e'; dump = d' } ->
            run_t
              t0
              { stack = s'
              ; env = e'
              ; ctl =
                  (fun { stack = s''; env = e''; dump = d'' } ->
                    run_a { stack = s''; env = e''; ctl = c; dump = d'' })
              ; dump = d'
              })
      ; dump = d
      }
  | TmOp f -> c { stack = (Primitive f, typ) :: s; env = e; dump = d }
  | TmFst ->
    (match s with
     | (Pair (v1, _), TyPair (ty1, _)) :: tl ->
       c { stack = (v1, ty1) :: tl; env = e; dump = d }
     | _ -> Error (`OperationalError "cannot project without a pair"))
  | TmSnd ->
    (match s with
     | (Pair (_, v2), TyPair (_, ty2)) :: tl ->
       c { stack = (v2, ty2) :: tl; env = e; dump = d }
     | _ -> Error (`OperationalError "cannot project without a pair"))
  | TmPair ->
    (match s with
     | (v1, ty1) :: (v2, ty2) :: tl ->
       c { stack = (Pair (v1, v2), TyPair (ty1, ty2)) :: tl; env = e; dump = d }
     | _ -> Error (`OperationalError "need two values on the stack to form a pair"))

and run_a : t -> final =
  fun ({ stack = s; env = e; ctl = c; dump = d } : t) ->
  match s with
  | (Primitive f, _) :: (Int a, TyInt) :: tl ->
    c { stack = (Int (f a), TyInt) :: tl; env = e; dump = d }
  | (Closure (e, id, t), typ) :: (v1, tyv1) :: tl ->
    if typ = tyv1
    then
      run_t
        t
        { stack = []
        ; env = (id, v1, tyv1) :: e
        ; ctl = (fun { stack = s'; env = _; dump = d' } -> d' s')
        ; dump =
            (fun s ->
              let v = List.hd s in
              c { stack = v :: tl; env = e; dump = d })
        }
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
  let c = get_or control (fun { stack = s; env = _; dump = d } -> d s) in
  let d =
    get_or dump (fun s ->
      match s with
      | [] -> Ok (Unit, TyUnit)
      | (h, typ) :: _ -> Ok (h, typ))
  in
  run_t t (init s e c d)
;;
