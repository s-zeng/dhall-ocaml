open! Core
open Angstrom

let number base digitParser =
  many1 digitParser
  >>| fun lst ->
  List.fold
    ~init:Bigint.zero
    ~f:Bigint.(fun accum digit -> (base * accum) + of_int (Char.get_digit_exn digit))
    lst
;;

let digit = satisfy Char.is_digit
let decimal = number (Bigint.of_int 10) digit
let oneOf lst = List.map ~f:char lst |> choice
