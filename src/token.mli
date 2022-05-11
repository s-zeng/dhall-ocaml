open! Core

val endOfLine : string Angstrom.t
val validCodePoint : int -> bool
val whitespace : unit Angstrom.t
val nonemptyWhitespace : unit Angstrom.t
val alpha : char -> bool
val digit : char -> bool
val alphaNum : char -> bool
val hexdig : char -> bool
val signPrefix : (int -> int) Angstrom.t
