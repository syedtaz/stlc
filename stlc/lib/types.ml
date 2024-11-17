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
  | TmOp of (int -> int -> int)
  | TmVar of int
  | TmAbs of string * ty * term
  | TmApp of term * term

let rec show_term t =
  match t with
  | TmUnit -> "λ.()"
  | TmInt x -> Format.sprintf "λ.%d" x
  | TmBool b -> Format.sprintf "λ.%b" b
  | TmOp _ -> "λx.(λy.(f x y))"
  | TmVar i -> Format.sprintf "[%d/x]" i
  | TmApp (a, b) -> Format.sprintf "(%s)(%s)" (show_term a) (show_term b)
  | TmAbs (id, ty, t) -> Format.sprintf "λ%s : %s. %s" id (show_ty ty) (show_term t)
;;

type tycontext = (string * binding) list

and binding =
  | NameBind
  | VarBind of ty

let add_binding (ctx : tycontext) (n : string) (ty : binding) = (n, ty) :: ctx

let get_type (ctx : tycontext) (i : int) =
  let _, res = List.nth ctx i in
  match res with
  | VarBind ty -> ty
  | _ -> raise (Invalid_argument "Bad context")
;;

let rec typeof ctx t =
  let ( >>= ) = Option.bind in
  match t with
  | TmUnit -> Some TyUnit
  | TmInt _ -> Some TyInt
  | TmBool _ -> Some TyBool
  | TmOp _ -> Some (TyArr (TyArr (TyInt, TyInt), TyInt))
  | TmVar i -> Some (get_type ctx i)
  | TmAbs (id, tyt1, t2) ->
    let ctx' = add_binding ctx id (VarBind tyt1) in
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

type control =
  | Apply
  | Term of term

type value =
  | Int of int
  | Bool of bool
  | Unit
  | Binop of (int -> int -> int)
  | Closure of (string * value) list * string * term

let rec show_value v =
  match v with
  | Int i -> Format.sprintf "%d : Int" i
  | Bool b -> Format.sprintf "%b : Bool" b
  | Unit -> "() : Unit"
  | Binop _ -> "f : int -> int -> int"
  | Closure (bv, id, e) ->
    Format.sprintf
      "{env: %s, id: %s, e: %s} : Closure "
      ("["
       ^ List.fold_left
           (fun acc (id, v) -> acc ^ Format.sprintf "(%s = %s)" id (show_value v))
           ""
           bv
       ^ "]")
      id
      (show_term e)
;;
