type 'l t = (string * Typechecker.ty, 'l) Vec.t

let empty : 'l t = Vec.[]

let add ctx pair : 'l t =
  let open Vec in
  pair :: ctx
;;
