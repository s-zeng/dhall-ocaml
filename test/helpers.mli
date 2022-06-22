open! Core

(** Given an Angstrom parser and lists of strings where the parser should match
    and should fail, prints the result of running this parser on each of the
    strings in the lists. Meant to be used with let%expect_test *)
val test_parses
  :  (module Sexpable with type t = 't)
  -> parser:'t Angstrom.t
  -> successes:string list
  -> failures:string list
  -> unit
