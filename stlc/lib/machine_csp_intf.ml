open Types

type stack = (value * ty) list
type env = (string * value * ty) list
type final = (value * ty, err) Result.t
type dump = stack -> final
type ctl = stack * env * dump -> final


module type Intf = sig
  type t

  val init : ?c:ctl option -> ?d:dump option -> stack -> env -> t

  val run : ?debug:bool -> term -> t -> final
  val show : t -> string
end
