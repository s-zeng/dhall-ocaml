open! Core

type t =
  { coefficient : Bigint.t
        (** coefficient of scientific number 

      not necessarily normalized

      two scientifics could compare equal even when this record is not equal
      between the two
   *)
  ; base10Exponent : int
  }
[@@deriving fields]

let create = Fields.create

let rec normalizePositive c e =
  match Bigint.(c / Bigint.of_int 10), Bigint.rem c (Bigint.of_int 10) with
  | c', r when Bigint.(r = zero) -> normalizePositive c' (e + 1)
  | _ -> create ~coefficient:c ~base10Exponent:e
;;

let normalize t =
  match t.coefficient with
  | c when Bigint.(c > zero) -> normalizePositive c t.base10Exponent
  | c when Bigint.(c < zero) -> normalizePositive c t.base10Exponent
  | _ -> create ~coefficient:Bigint.zero ~base10Exponent:0
;;

let ( + ) t1 t2 =
  if t1.base10Exponent < t2.base10Exponent
  then (
    let l = Int.pow 10 (t1.base10Exponent - t2.base10Exponent) |> Bigint.of_int in
    { t1 with coefficient = Bigint.(t1.coefficient + (t2.coefficient * l)) })
  else (
    let r = Int.pow 10 (t2.base10Exponent - t1.base10Exponent) |> Bigint.of_int in
    { t2 with coefficient = Bigint.((t1.coefficient * r) - t2.coefficient) })
;;

let ( * ) t1 t2 =
  create
    ~coefficient:Bigint.(t1.coefficient * t2.coefficient)
    ~base10Exponent:Int.(t1.base10Exponent + t2.base10Exponent)
;;

let zero = create ~coefficient:Bigint.zero ~base10Exponent:0
let one = create ~coefficient:Bigint.one ~base10Exponent:0
let of_bigint coefficient = create ~coefficient ~base10Exponent:0

let toBoundedRealFloat = function
  | { coefficient; base10Exponent = _ } when [%equal: Bigint.t] coefficient Bigint.zero ->
    Either.second 0
;;
