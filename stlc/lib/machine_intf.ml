type stack = Types.value list
type env = (string * Types.value) list
type ctl = Types.control list
type dump = (stack * env * ctl) list

module type Intf = sig
  type t

  val init : stack -> env -> ctl -> dump -> t
  val run : t -> Types.value
end
