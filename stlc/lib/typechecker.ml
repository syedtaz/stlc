type ty =
  | TyInt
  | TyArr of ty * ty

let rec show_ty x =
  match x with
  | TyInt -> "int"
  | TyArr (t1, t2) ->
    let st1 = show_ty t1 in
    let st2 = show_ty t2 in
    Format.sprintf "%s -> %s" st1 st2
;;

type term =
  | TmInt of int
  | TmVar of int
  | TmAbs of string * ty * term
  | TmApp of term * term

type context = (string * binding) list

and binding =
  | NameBind
  | VarBind of ty

let add_binding (ctx : context) (n : string) (ty : binding) = (n, ty) :: ctx

let get_type (ctx : context) (i : int) =
  let _, res = List.nth ctx i in
  match res with
  | VarBind ty -> ty
  | _ -> raise (Invalid_argument "Bad context")
;;

let rec typeof ctx t =
  match t with
  | TmInt _ -> TyInt
  | TmVar i -> get_type ctx i
  | TmAbs (id, tyt1, t2) ->
    let ctx' = add_binding ctx id (VarBind tyt1) in
    let tyt2 = typeof ctx' t2 in
    TyArr (tyt1, tyt2)
  | TmApp (t1, t2) ->
    let tyt1 = typeof ctx t1 in
    let tyt2 = typeof ctx t2 in
    (match tyt1 with
     | TyArr (tyT1, tyT2) ->
       if tyT1 = tyt2 then tyT2 else raise (Invalid_argument "type error")
     | _ -> raise (Invalid_argument "type error"))
;;
