open! Core
open! Dhall

let whole_parse ~parser = Angstrom.parse_string ~consume:Angstrom.Consume.All parser

let test_parses
    (type t)
    (module T : Sexpable.S with type t = t)
    ~(parser : T.t Angstrom.t)
    ~successes
    ~failures
  =
  let successes = List.map ~f:(whole_parse ~parser) successes in
  let failures = List.map ~f:(whole_parse ~parser) failures in
  print_s [%message (successes : (T.t, string) Result.t list)];
  print_s [%message (failures : (T.t, string) Result.t list)]
;;

let%expect_test "endOfLine" =
  test_parses
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
  test_parses
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
  test_parses
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
