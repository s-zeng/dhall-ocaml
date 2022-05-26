(* Port of haskell's Data.Scientific *)
open! Core

type t

val create : coefficient:Bigint.t -> base10Exponent:int -> t
