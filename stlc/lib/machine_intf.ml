type 'a stack = Types.value list
type 'a env = (string * Types.value) list
type ctl = Types.control list
type 'a dump = ('a stack * 'a env * ctl) list

module type Intf = sig
  type t

  val init : 'a stack -> 'a env -> ctl -> 'a dump -> t
  val run : t -> Types.value
end
