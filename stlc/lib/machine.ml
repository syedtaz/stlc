module Env = struct
  type 'a t = (string * 'a) list
end

module Term = struct
  type t =
    | Var of string
    | Lam of string * t
    | App of t * t
end

module Value = struct
  type t =
    | Unit
    | Nat of int
    | Boolean of bool
    | Closure of
        { env : t Env.t
        ; bv : string list
        ; control : Term.t
        }
end

module Control = struct
  type t =
    | Apply
    | Term of Term.t
end

type t =
  { stack : Value.t list
  ; env : Value.t Env.t
  ; control : Control.t list
  ; dump : (Value.t list * Value.t Env.t * Control.t list) list
  }