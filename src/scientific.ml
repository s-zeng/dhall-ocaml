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
[@@deriving fields, sexp]

let create = Fields.create
let big10 = Bigint.of_int 10

let rec normalizePositive c e =
  match Bigint.(c / big10), Bigint.rem c big10 with
  | c', r when Bigint.(r = zero) -> normalizePositive c' (e + 1)
  | _ -> create ~coefficient:c ~exponent:e
;;

let normalize t =
  match t.coefficient with
  | c when Bigint.(c > zero) -> normalizePositive c t.exponent
  | c when Bigint.(c < zero) -> normalizePositive (Bigint.neg c) t.exponent
  | _ -> create ~coefficient:Bigint.zero ~exponent:0
;;

let ( + ) t1 t2 =
  if t1.exponent < t2.exponent
  then (
    let l = Bigint.pow big10 (Bigint.of_int (t2.exponent - t1.exponent)) in
    { t1 with coefficient = Bigint.(t1.coefficient + (t2.coefficient * l)) })
  else (
    let r = Bigint.pow big10 (Bigint.of_int (t1.exponent - t2.exponent)) in
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

let toBoundedRealFloat =
  (* we hardcode to Double values from the haskell code *)
  let radix = 2 in
  let digits = 53 in
  let lo, hi = -1021, 1024 in
  let log10Radix = Float.log10 (Float.of_int radix) in
  let hilimit = Float.round_up Float.(of_int hi * log10Radix) in
  let lolimit =
    Float.(round_down (of_int lo * log10Radix) - round_up (of_int digits * log10Radix))
  in
  function
  | { coefficient; exponent = _ } when [%equal: Bigint.t] coefficient Bigint.zero ->
    Either.second Float.zero
  | { coefficient; exponent } when exponent > limit ->
    if Float.(of_int exponent > hilimit)
    then Either.first Float.infinity
    else
      Either.second
        (Bigint.to_float coefficient
        *. Bigint.to_float (Bigint.pow big10 (Bigint.of_int exponent)))
  | { coefficient; exponent } when exponent < -limit ->
    let d =
      coefficient |> Bigint.abs |> Bigint.to_float |> Float.log10 |> Float.round_down
    in
    if Float.(of_int exponent < lolimit) && Float.(of_int exponent + d < lolimit)
    then Either.first (Float.copysign 0. (Bigint.to_float coefficient))
    else Either.second Float.(Bigint.to_float coefficient / (10. ** -of_int exponent))
  | { coefficient; exponent } when exponent < 0 ->
    Either.second Float.(Bigint.to_float coefficient / (10. ** -of_int exponent))
  | { coefficient; exponent } ->
    Either.second
      Float.(
        Bigint.to_float coefficient
        * (Bigint.pow big10 (Bigint.of_int exponent) |> Bigint.to_float))
;;

let toRealFloat = Fn.compose Either.value toBoundedRealFloat
