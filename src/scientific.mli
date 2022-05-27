(* Port of haskell's Data.Scientific *)
open! Core

type t

val create : coefficient:Bigint.t -> exponent:int -> t
val coefficient : t -> Bigint.t
val exponent : t -> int
val normalize : t -> t
val ( + ) : t -> t -> t
val ( * ) : t -> t -> t
val zero : t
val one : t
val of_bigint : Bigint.t -> t
