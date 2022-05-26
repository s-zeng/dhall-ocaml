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
