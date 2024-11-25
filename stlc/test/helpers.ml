open Stlc.Types

let extract_int v =
  match v with
  | Ok (Int x, _) -> Some x
  | _ -> None
;;

let extract_bool v =
  match v with
  | Ok (Bool x, _) -> Some x
  | _ -> None
;;

let must_be_error (v : ('a, err) result) =
  match v with
  | Error `TypeError -> Some "typerror"
  | Error (`OperationalError s) -> Some s
  | _ -> None
;;