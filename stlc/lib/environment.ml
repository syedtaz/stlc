(** A diff list that holds variable bindings.

    Thanks to https://drup.github.io/2016/08/02/difflists/ *)

type ('hd, 'tl) t =
  | Nil : ('hd, 'tl) t
  | Cons : 'hd * ('tl, 'a) t -> ('hd, 'tl * 'a) t

let destruct_exn : type hd tl a. (hd, tl * a) t -> hd * (tl, a) t =
  fun lst ->
  match lst with
  | Nil -> failwith "empty list" (* Maybe add length type? *)
  | Cons (hd, tl) -> hd, tl
;;
