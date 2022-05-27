(** Code ported from haskell's Text.Parser.Token *)

open! Core

val number : Bigint.t -> char Angstrom.t -> Bigint.t Angstrom.t
val digit : char Angstrom.t
val decimal : Bigint.t Angstrom.t
val oneOf : char list -> char Angstrom.t
