open! Core
open! Dhall

let%expect_test "endOfLine" =
  Helpers.test_parses
    (module String)
    ~parser:Token.endOfLine
    ~successes:[ "\n"; "\r\n" ]
    ~failures:[ ""; " "; " x" ];
  [%expect
    {|
    (successes ((Ok "\n") (Ok "\r\n")))
    (failures
     ((Error "newline: not enough input") (Error "newline: not enough input")
      (Error "newline: string"))) |}]
;;

let%expect_test "whitespace" =
  Helpers.test_parses
    (module Unit)
    ~parser:Token.whitespace
    ~successes:[ ""; " "; "\t"; "\n"; "-- line comment\n"; "{- block comment -}" ]
    ~failures:[ "hmm"; "wow"; "-- comment no newline"; "3 + {- block comment -} 4" ];
  [%expect
    {|
    (successes ((Ok ()) (Ok ()) (Ok ()) (Ok ()) (Ok ()) (Ok ())))
    (failures
     ((Error ": end_of_input") (Error ": end_of_input") (Error ": end_of_input")
      (Error ": end_of_input"))) |}]
;;

let%expect_test "nonemptyWhitespace" =
  Helpers.test_parses
    (module Unit)
    ~parser:Token.nonemptyWhitespace
    ~successes:[ " "; "\t"; "\n"; "-- line comment\n"; "{- block comment -}" ]
    ~failures:[ ""; "hmm"; "wow"; "-- comment no newline"; "3 + {- block comment -} 4" ];
  [%expect
    {|
    (successes ((Ok ()) (Ok ()) (Ok ()) (Ok ()) (Ok ())))
    (failures
     ((Error "nonemptyWhitespace > whitespaceChunk: no more choices")
      (Error "nonemptyWhitespace > whitespaceChunk: no more choices")
      (Error "nonemptyWhitespace > whitespaceChunk: no more choices")
      (Error "nonemptyWhitespace > whitespaceChunk: no more choices")
      (Error "nonemptyWhitespace > whitespaceChunk: no more choices"))) |}]
;;

let%expect_test "doubleLiteral" =
  Helpers.test_parses
    (module Float)
    ~parser:Token.doubleLiteral
    ~successes:[ "58.0"; "4e2"; "-123.456e-67"; "0.1E4"; "3.4028234"; "3.4028234e38" ]
    ~failures:[ "58."; "0"; "str"; "" ];
  [%expect
    {|
    (successes
     ((Ok 58) (Ok 400) (Ok -1.2345599999999999E-65) (Ok 1000) (Ok 3.4028234)
      (Ok 3.4028234E+38)))
    (failures
     ((Error "doubleLiteral: no more choices")
      (Error "doubleLiteral: no more choices")
      (Error "doubleLiteral: satisfy: 's'")
      (Error "doubleLiteral: not enough input"))) |}]
;;
