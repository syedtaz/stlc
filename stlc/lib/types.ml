type ty =
  | TyUnit
  | TyInt
  | TyBool
  | TyArr of ty * ty

let rec show_ty x =
  match x with
  | TyUnit -> "()"
  | TyInt -> "int"
  | TyBool -> "bool"
  | TyArr (t1, t2) ->
    let st1 = show_ty t1 in
    let st2 = show_ty t2 in
    Format.sprintf "%s -> %s" st1 st2
;;

type term =
  | TmUnit
  | TmInt of int
  | TmBool of bool
  | TmOp of (int -> int)
  | TmVar of int
  | TmAbs of string * ty * term
  | TmApp of term * term

let rec show_term t =
  match t with
  | TmUnit -> "λ.()"
  | TmInt x -> Format.sprintf "λ.%d" x
  | TmBool b -> Format.sprintf "λ.%b" b
  | TmOp _ -> "λx.(f x)"
  | TmVar i -> Format.sprintf "[%d/]" i
  | TmApp (a, b) -> Format.sprintf "(%s)(%s)" (show_term a) (show_term b)
  | TmAbs (id, _, t) -> Format.sprintf "λ%s. %s" id (show_term t)
;;

type value =
  | Int of int
  | Bool of bool
  | Unit
  | Primitive of (int -> int)
  | Closure of (string * value * ty) list * string * term

type context = (string * ty) list

let context_of_env lst = List.map (fun (id, _, ty) -> (id, ty)) lst

let get_type (ctx : context) (i : int) =
  let _, ty = List.nth ctx i in
  ty
;;

let rec typeof ctx t =
  let ( >>= ) = Option.bind in
  match t with
  | TmUnit -> Some TyUnit
  | TmInt _ -> Some TyInt
  | TmBool _ -> Some TyBool
  | TmOp _ -> Some (TyArr (TyInt, TyInt))
  | TmVar i -> Some (get_type ctx i)
  | TmAbs (id, tyt1, t2) ->
    let ctx' = (id, tyt1) :: ctx in
    typeof ctx' t2 >>= fun tyt2 -> Some (TyArr (tyt1, tyt2))
  | TmApp (t1, t2) ->
    typeof ctx t1
    >>= fun tyt1 ->
    typeof ctx t2
    >>= fun tyt2 ->
    (match tyt1 with
     | TyArr (tyT1, tyT2) -> if tyT1 = tyt2 then Some tyT2 else None
     | _ -> None)
;;

let typecheck_exn ctx t = typeof ctx t |> Option.get

type control =
  | Apply
  | Term of term

let show_control c =
  match c with
  | Apply -> "apply"
  | Term x -> show_term x
;;

let rec show_value v =
  match v with
  | Int i -> Format.sprintf "%d : Int" i
  | Bool b -> Format.sprintf "%b : Bool" b
  | Unit -> "() : Unit"
  | Primitive _ -> "int -> int"
  | Closure (bv, _, e) ->
    Format.sprintf
      "{env: %s, e: %s}"
      ("["
       ^ List.fold_left
           (fun acc (id, v, _) -> acc ^ Format.sprintf "(%s = %s)" id (show_value v))
           ""
           bv
       ^ "]")
      (show_term e)
;;
