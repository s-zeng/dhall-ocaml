open! Core

val endOfLine : string Angstrom.t
val validCodePoint : int -> bool
val whitespace : unit Angstrom.t
val lineComment : string Angstrom.t
val lineCommentPrefix : string Angstrom.t
val blockComment : string Angstrom.t
val nonemptyWhitespace : unit Angstrom.t
val alpha : char -> bool
val digit : char -> bool
val alphaNum : char -> bool
val hexdig : char -> bool

module type Negatable = sig
  type t

  val neg : t -> t
end

val signPrefix : (module Negatable with type t = 't) -> ('t -> 't) Angstrom.t
val doubleLiteral : float Angstrom.t
val doubleInfinity : float Angstrom.t
val naturalLiteral : Bigint.t Angstrom.t
