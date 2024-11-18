type stack = (Types.value * Types.ty) list
type env = (string * Types.value * Types.ty) list
type ctl = Types.control list
type dump = (stack * env * ctl) list

module type Intf = sig
  type t

  val init : stack -> env -> ctl -> dump -> t
  val run : ?debug:(bool) -> t -> Types.value * Types.ty

  val show : t -> string
end
