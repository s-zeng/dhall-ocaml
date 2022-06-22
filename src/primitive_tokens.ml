open! Core
open Angstrom

let number ~base ~digitParser ~digitToNum =
  many1 digitParser
  >>| (fun lst ->
        List.fold
          ~init:Bigint.zero
          ~f:Bigint.(fun accum digit -> (base * accum) + digitToNum digit)
          lst)
  <?> "number (base " ^ Bigint.to_string base ^ ")"
;;

let digit = satisfy Char.is_digit <?> "digit"

let decimal =
  number
    ~base:(Bigint.of_int 10)
    ~digitParser:digit
    ~digitToNum:(Fn.compose Bigint.of_int Char.get_digit_exn)
  <?> "decimal"
;;

let oneOf lst =
  List.map ~f:char lst |> choice <?> "oneOf (" ^ String.of_char_list lst ^ ")"
;;

let hexdigit =
  digit <|> oneOf (String.to_list "abcdef") <|> oneOf (String.to_list "ABCDEF")
;;

let hexadecimal =
  number
    ~base:(Bigint.of_int 16)
    ~digitParser:hexdigit
    ~digitToNum:(Fn.compose Bigint.of_int Char.get_hex_digit_exn)
  <?> "hexadecimal"
;;
