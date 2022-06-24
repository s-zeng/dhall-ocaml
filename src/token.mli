(** Fairly direct translation of Dhall.Parser.Token *)
open! Core

(** Match an end-of-line character sequence *)
val endOfLine : string Angstrom.t

(** Returns true if the given int is a valid unicode codepoint*)
val validCodePoint : int -> bool

(** Parse 0 or more whitespace characters, including comments

    Corresponds to the @whsp@ rule in the official grammar
 *)
val whitespace : unit Angstrom.t

(** Parse a Dhall single-line comment, starting from `--` and until the last
    character of the line /before/ the end-of-line character *)
val lineComment : string Angstrom.t

(** Same as `lineComment` except that this doesn't parse the end-of-line character *)
val lineCommentPrefix : string Angstrom.t

val blockComment : string Angstrom.t

(** Parse 1 or more whitespace characters, including comments

    Corresponds to the @whsp1@ rule in the official grammar
*)
val nonemptyWhitespace : unit Angstrom.t

val alpha : char -> bool
val digit : char -> bool
val alphaNum : char -> bool

(** Parse a hex digit (upper or lowercase)

    Corresponds to the @HEXDIG@ rule in the official grammar
*)
val hexdig : char -> bool

module type Negatable = sig
  type t

  (** law: neg (neg x) == x *)
  val neg : t -> t
end

(** Parses a leading @+@ or @-@ sign *)
val signPrefix : (module Negatable with type t = 't) -> ('t -> 't) Angstrom.t

(** Parse a `Dhall.Syntax.Double` literal 

    Corresponds to the @double-literal@ rule from the official grammar
 *)
val doubleLiteral : float Angstrom.t

(** Parse a signed @Infinity@ 

    Corresponds to the @minus-infinity-literal@ and
    @plus-infinity-literal@ rules from the official grammar
 *)
val doubleInfinity : float Angstrom.t

(** Parse a `Dhall.Syntax.Natural` literal 

    Corresponds to the @natural=literal@ rule from the officical grammar
 *)
val naturalLiteral : Bigint.t Angstrom.t

(** Parse a 4-digit year

    Corresponds to the @date-fullyear@ rule from the official grammar
 *)
val dateFullYear : int Angstrom.t
