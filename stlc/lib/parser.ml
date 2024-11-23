open Angstrom
open Newtypes.Term

let identifier =
  satisfy (function
    | 'a' .. 'z' | 'A' .. 'Z' -> true
    | _ -> false)
;;

let number =
  satisfy (function
    | '0' .. '9' -> true
    | _ -> false)
  >>= fun x -> return (TmInt (int_of_char x - int_of_char '0'))
;;

let lambda = string "λ" *> identifier *> char '.'
let incr = string "incr" *> return (TmPrim `incr)
let decr = string "decr" *> return (TmPrim `decr)
let unit = lambda *> string "()" *> return TmUnit
let primitive = incr <|> decr
let term = primitive <|> unit <|> number
let parse = parse_string ~consume:Consume.All term
