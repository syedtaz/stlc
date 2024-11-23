(** Reworking the AST type... *)

module Term = struct
  type t =
    | TmPrim of [ `incr | `decr ]
    | TmUnit
    | TmInt of int

  let show t =
    match t with
    | TmPrim `incr -> "TmPrim(incr)"
    | TmPrim `decr -> "TmPrim(decr)"
    | TmUnit -> "TmUnit"
    | TmInt x -> Format.sprintf "TmInt(%d)" x
  ;;
end
