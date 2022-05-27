open! Core

type t =
  { coefficient : Bigint.t
        (** coefficient of scientific number 

      not necessarily normalized

      two scientifics could compare equal even when this record is not equal
      between the two
   *)
  ; exponent : int
  }
[@@deriving fields]

let create = Fields.create

let rec normalizePositive c e =
  match Bigint.(c / Bigint.of_int 10), Bigint.rem c (Bigint.of_int 10) with
  | c', r when Bigint.(r = zero) -> normalizePositive c' (e + 1)
  | _ -> create ~coefficient:c ~exponent:e
;;

let normalize t =
  match t.coefficient with
  | c when Bigint.(c > zero) -> normalizePositive c t.exponent
  | c when Bigint.(c < zero) -> normalizePositive c t.exponent
  | _ -> create ~coefficient:Bigint.zero ~exponent:0
;;

let ( + ) t1 t2 =
  if t1.exponent < t2.exponent
  then (
    let l = Int.pow 10 (t1.exponent - t2.exponent) |> Bigint.of_int in
    { t1 with coefficient = Bigint.(t1.coefficient + (t2.coefficient * l)) })
  else (
    let r = Int.pow 10 (t2.exponent - t1.exponent) |> Bigint.of_int in
    { t2 with coefficient = Bigint.((t1.coefficient * r) - t2.coefficient) })
;;

let ( * ) t1 t2 =
  create
    ~coefficient:Bigint.(t1.coefficient * t2.coefficient)
    ~exponent:Int.(t1.exponent + t2.exponent)
;;

let zero = create ~coefficient:Bigint.zero ~exponent:0
let one = create ~coefficient:Bigint.one ~exponent:0
let of_bigint coefficient = create ~coefficient ~exponent:0

let limit = 324

let toBoundedRealFloat = function
  | { coefficient; exponent = _ } when [%equal: Bigint.t] coefficient Bigint.zero ->
    Either.second 0
  | { coefficient = _; exponent } when exponent > limit ->
    Either.second 0

;;
