(** Code ported from haskell's Text.Parser.Token *)

open! Core

val number
  :  base:Bigint.t
  -> digitParser:'a Angstrom.t
  -> digitToNum:('a -> Bigint.t)
  -> Bigint.t Angstrom.t

val digit : char Angstrom.t
val decimal : Bigint.t Angstrom.t
val oneOf : char list -> char Angstrom.t
val hexdigit : char Angstrom.t
val hexadecimal : Bigint.t Angstrom.t
