(** Reworking the AST type... *)

module Term = struct
  type t = TmPrim of [ `incr | `decr ]

  let show t =
    match t with
    | TmPrim `incr -> "TmPrim(incr)"
    | TmPrim `decr -> "TmPrim(decr)"
  ;;
end
