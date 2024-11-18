open Types

type stack = (value * ty) list
type env = (string * value * ty) list
type ctl = control list
type dump = (stack * env * ctl) list
type final = (value * ty, error) Result.t

module type Intf = sig
  type t

  val init : stack -> env -> ctl -> dump -> t
  val run : ?debug:bool -> t -> final
  val show : t -> string
end
