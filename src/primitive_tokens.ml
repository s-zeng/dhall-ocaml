open! Core
open Angstrom

let number base baseDigit =
  many1 baseDigit
  >>| List.fold_left
        ~init:Bigint.zero
        ~f:Bigint.(fun x d -> (base * x) + of_int (Char.to_int d))
;;

let digit = satisfy Char.is_digit
let decimal = number (Bigint.of_int 10) digit

let oneOf lst = List.map ~f:char lst |> choice
