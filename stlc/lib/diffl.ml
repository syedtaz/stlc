(* open Types

(* This was an attempt at writing an SECD machine where the stack is indexed
   using a length and the dump is a difflist indexed by its length
   and the length of its head and tail. Unfortunately, I don't think I really
   reduced any code in the evaluator -- and I can't run the tests cause I can't
   seem to write a stack_of_list function so that I can initialize the machine. :(

   It was fun writing this though! Learnt a bunch. *)

type z = Z
type 'n s = S : 'n -> 'n s

module Stack = struct
  type 'length t =
    | Nil : z t
    | Cons : value * 'l t -> 'l s t
end

type env = (string * value) list
type ctl = control list
type 'l snapshot = 'l Stack.t * env * ctl

module SStack = struct
  type ('l1, 'l2, 'l) t =
    | Nil : (_, _, z) t
    | Cons : 'l snapshot * ('lhd, 'ltl, 'n) t -> ('l, 'lhd * 'ltl, 'n s) t
end

type ('sl, 'dhl, 'dtl, 'dl) t =
  { stack : 'sl Stack.t
  ; env : env
  ; control : ctl
  ; dump : ('dhl, 'dtl, 'dl) SStack.t
  }

let rec run : type sl dhl dtl dl. ?debug:bool -> (sl, dhl, dtl, dl) t -> value =
  fun ?debug lst ->
  let _ = debug in
  match lst with
  (* If control and dump is empty, then the result of the evaluation is on the
     stack. *)
  | { stack = Nil; env = _; control = []; dump = _ } -> failwith "no op"
  | { stack = Cons (el, _); env = _; control = []; dump = Nil } -> el
  (* If the control is empty but the dump is not empty, take the result of the
     evaluation from the current stack and restore the head of the dump.*)
  | { stack = Cons (el, _); env = _; control = []; dump = Cons (hd, tl) } ->
    let s', e', c' = hd in
    run { stack = Cons (el, s'); env = e'; control = c'; dump = tl }
  | { stack = s; env = e; control = ctl_hd :: ctl_tl; dump = d } ->
    (match ctl_hd with
     | Term t ->
       (match t with
        | TmInt i -> run { stack = Cons (Int i, s); env = e; control = ctl_tl; dump = d }
        | TmBool b ->
          run { stack = Cons (Bool b, s); env = e; control = ctl_tl; dump = d }
        | TmUnit -> run { stack = s; env = e; control = ctl_tl; dump = d }
        | TmVar i ->
          let _, vl = List.nth e i in
          run { stack = Cons (vl, s); env = e; control = ctl_tl; dump = d }
        | TmApp (t1, t2) ->
          run
            { stack = s
            ; env = e
            ; control = Term t2 :: Term t1 :: Apply :: ctl_tl
            ; dump = d
            }
        | TmAbs (id, _, body) ->
          run
            { stack = Cons (Closure (e, id, body), s)
            ; env = e
            ; control = ctl_tl
            ; dump = d
            }
        | TmOp f ->
          run { stack = Cons (Primitive f, s); env = e; control = ctl_tl; dump = d })
     | Apply ->
       (match s with
        | Nil -> failwith "can't apply when stack is empty"
        | Cons (op, s') ->
          (match op with
           | Int _ | Unit | Bool _ -> failwith "invalid operator"
           | Primitive f ->
             (match s' with
              | Nil -> failwith "invalid operator"
              | Cons (v1, tl) ->
                (match v1 with
                 | Int a ->
                   run
                     { stack = Cons (Int (f a), tl); env = e; control = ctl_tl; dump = d }
                 | _ -> failwith "invalid operator"))
           | Closure (e, id, t) ->
             (match s' with
              | Nil -> failwith "invalid closure"
              | Cons (v1, tl) ->
                run
                  { stack = Nil
                  ; env = (id, v1) :: e
                  ; control = [ Term t ]
                  ; dump = Cons ((tl, e, ctl_tl), d)
                  }))))
;; *)
