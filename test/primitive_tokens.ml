open! Core
open! Dhall

let%expect_test "decimal" =
  Helpers.test_parses
    (module Bigint)
    ~parser:Primitive_tokens.decimal
    ~successes:[ "123"; "056"; "991919191912321358" ]
    ~failures:[ "0x456"; ""; "str"; "1.2" ];
  [%expect
    {|
     (successes ((Ok 123) (Ok 56) (Ok 991919191912321358)))
     (failures
      ((Error ": end_of_input")
       (Error "decimal > number (base 10) > digit: not enough input")
       (Error "decimal > number (base 10) > digit: satisfy: 's'")
       (Error ": end_of_input"))) |}]
;;

let%expect_test "hexadecimal" =
  Helpers.test_parses
    (module Bigint)
    ~parser:Primitive_tokens.hexadecimal
    ~successes:[ "456"; "010"; "deadbeef" ]
    ~failures:[ "deadsleep"; "dead.beef" ];
  [%expect
    {|
     (successes ((Ok 1110) (Ok 16) (Ok 3735928559)))
     (failures ((Error ": end_of_input") (Error ": end_of_input"))) |}]
;;
