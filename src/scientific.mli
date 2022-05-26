(* Port of haskell's Data.Scientific *)
open! Core

type t

include Comparable.S with type t := t

val create : coefficient:Bigint.t -> base10Exponent:int -> t
val base10Exponent : t -> int
val ( + ) : t -> t -> t
