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

let rec normalizePositive c e = match (Bigint.(c / (Bigint.of_int 10)), Bigint.rem c (Bigint.of_int 10)) with
  | (c', r) when Bigint.(r = zero) -> normalizePositive c' (e+1)
  | _ -> create ~coefficient:c ~base10Exponent:e

let normalize t = match t.coefficient with
    | c when Bigint.(c > zero) -> normalizePositive c t.base10Exponent
    | c when Bigint.(c < zero) -> normalizePositive c t.base10Exponent
    | _ -> create ~coefficient:Bigint.zero ~base10Exponent:0

module X = Comparable.Make

(* let ( + ) (s1 : t) (s2 : t) = *)
(*   if s1 < s2 *)
(*   then ( *)
(*     let l = magnitude (s1.base10Exponent - s2.base10Exponent) in *)
(*     { s1 with coefficient = s1.coefficient + (s2.coefficient * l) }) *)
(*   else ( *)
(*     let r = magnitude (s2.base10Exponent - s1.base10Exponent) in *)
(*     { s2 with coefficient = (s1.coefficient * r) - s2.coefficient }) *)
(* ;; *)
