type zero = Z
type 'n succ = S of 'n

type 'n nat =
  | Z : zero nat
  | S : 'n nat -> 'n succ nat

type any_nat = Any : 'n nat -> any_nat

let rec nat_of_int : int -> any_nat =
  fun (n : int) ->
  match n with
  | 0 -> Any Z
  | _ ->
    (match nat_of_int (n - 1) with
     | Any x -> Any (S x))
;;

type 'a fin =
  | FZ : 'a succ fin
  | FS : 'a fin -> 'a succ fin

type ('a, 'l) t =
  | [] : ('a, zero nat) t
  | ( :: ) : 'a * ('a, 'l nat) t -> ('a, 'l succ nat) t

let rec index : type n. ('a, n succ nat) t -> n fin -> 'a =
  fun vec i ->
  match i, vec with
  | FZ, hd :: _ -> hd
  | FS n, _ :: tl -> index tl n
;;

let ( !!! ) a b = index a b
