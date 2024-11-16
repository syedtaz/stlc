open Types

type control =
  | Apply
  | Term of term

type value =
  | Int of int
  | Bool of bool
  | Unit
  | Closure of (string * value) list * string * term

let show_value v = match v with
  | Int i -> Format.sprintf "%d : Int" i
  | Bool b -> Format.sprintf "%b : Bool" b
  | Unit -> "() : Unit"
  | Closure _ -> "Closure" (* Unimplemented *)

type t =
  { stack : value list
  ; env : (string * value) list
  ; control : control list
  ; dump : (value list * (string * value) list * control list) list
  }

let empty ctl = { stack = []; env = []; control = ctl; dump = [] }

let apply_op op v1 v2 =
  match op, v1, v2 with
  | TmOp f, TmInt a, TmInt b -> TmInt (f a b)
  | _ -> failwith "type error"
;;

let rec run (m : t) =
  match m with
  (* If control and dump is empty, then the result of the evaluation is on the
     stack. *)
  | { stack = s; env = _; control = []; dump = [] } -> List.hd s
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
        | TmInt i -> run { stack = Int i :: s; env = e; control = ctl_tl; dump = d }
        | TmBool b -> run { stack = Bool b :: s; env = e; control = ctl_tl; dump = d }
        | TmUnit -> run { stack = s; env = e; control = ctl_tl; dump = d }
        | TmVar i ->
          let _, vl = List.nth e i in
          run { stack = vl :: s; env = e; control = ctl_tl; dump = d }
        | TmApp (t1, t2) ->
          run
            { stack = s
            ; env = e
            ; control = Term t2 :: Term t1 :: Apply :: ctl_tl
            ; dump = d
            }
        | TmAbs (id, _, body) ->
          run { stack = Closure (e, id, body) :: s; env = e; control = ctl_tl; dump = d }
        | TmOp _ -> failwith "unimplemented")
     | Apply -> failwith "unimplemented")
;;
