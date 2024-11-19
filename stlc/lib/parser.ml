open Angstrom
open Newtypes.Term

let incr = string "incr" *> return (TmPrim `incr)
let decr = string "decr" *> return (TmPrim `decr)

let primitives = incr <|> decr

let parse = parse_string ~consume:Consume.All primitives