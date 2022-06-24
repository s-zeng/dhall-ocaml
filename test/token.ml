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
     ((Error "endOfLine: not enough input") (Error "endOfLine: not enough input")
      (Error "endOfLine: string"))) |}]
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

let%expect_test "lineComment" =
  Helpers.test_parses
    (module String)
    ~parser:Token.lineComment
    ~successes:[ "-- line comment\n" ]
    ~failures:[ "-- line comment no newline"; "{- block comment -}" ];
  [%expect
    {|
    (successes ((Ok "-- line comment")))
    (failures
     ((Error "lineComment > endOfLine: not enough input")
      (Error "lineComment: string"))) |}]
;;

let%expect_test "lineCommentPrefix" =
  Helpers.test_parses
    (module String)
    ~parser:Token.lineCommentPrefix
    ~successes:[ "-- line comment no newline" ]
    ~failures:[ "-- line comment\n"; "{- block comment -}" ];
  [%expect
    {|
    (successes ((Ok "-- line comment no newline")))
    (failures ((Error ": end_of_input") (Error ": string"))) |}]
;;

let%expect_test "blockComment" =
  Helpers.test_parses
    (module String)
    ~parser:Token.blockComment
    ~successes:[ "{- block comment! -}"; "{- multi\n line -}"; "{--}" ]
    ~failures:[ "-- line comment\n"; "{- incomplete" ];
  [%expect
    {|
    (successes
     ((Ok "{- block comment! -}") (Ok  "{- multi\
                                      \n line -}") (Ok {--})))
    (failures
     ((Error ": string")
      (Error
       "blockCommentContinue > blockCommentContinue > blockCommentChunk: no more choices")))
 |}]
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
     ((Error "doubleLiteral > oneOf (eE): no more choices")
      (Error "doubleLiteral > oneOf (eE): no more choices")
      (Error "doubleLiteral > decimal > number (base 10) > digit: satisfy: 's'")
      (Error
       "doubleLiteral > decimal > number (base 10) > digit: not enough input"))) |}]
;;

let%expect_test "doubleInfinity" =
  Helpers.test_parses
    (module Float)
    ~parser:Token.doubleInfinity
    ~successes:[ "Infinity"; "-Infinity" ]
    ~failures:[ "NaN"; "Inf.0"; "-Inf" ];
  [%expect
    {|
    (successes ((Ok INF) (Ok -INF)))
    (failures
     ((Error "doubleInfinity literal: not enough input")
      (Error "doubleInfinity literal: not enough input")
      (Error "doubleInfinity literal: not enough input"))) |}]
;;

let%expect_test "naturalLiteral" =
  Helpers.test_parses
    (module Bigint)
    ~parser:Token.naturalLiteral
    ~successes:[ "123123678928479823"; "0"; "0xdeadbeefabcdef" ]
    ~failures:[ "-4"; "3.2"; "base64" ];
  [%expect
    {|
    (successes ((Ok 123123678928479823) (Ok 0) (Ok 62678480406171119)))
    (failures
     ((Error "naturalLiteral: char '0'") (Error ": end_of_input")
      (Error "naturalLiteral: char '0'"))) |}]
;;

let%expect_test "dateFullYear" =
  Helpers.test_parses
    (module Int)
    ~parser:Token.dateFullYear
    ~successes:[ "2022"; "1937"; "4096"; "0002" ]
    ~failures:[ "2"; "5BC"; "672" ];
  [%expect
    {|
    (successes ((Ok 2022) (Ok 1937) (Ok 4096) (Ok 2)))
    (failures
     ((Error ": not enough input") (Error ": satisfy: 'B'")
      (Error ": not enough input"))) |}]
;;
