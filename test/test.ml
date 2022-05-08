open! Core

let%expect_test "test" =
  Dhall.main ();
  [%expect {| Hurrah! |}]
;;
