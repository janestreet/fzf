open! Core
open! Async
open! Import
open Test_helpers

let%expect_test "large input" =
  let%bind () = test "variable-input" [ "32769" ] [] in
  [%expect
    {|
      a
      a
      a
      a
      a
      a
      a
    > a
      32769/32769
    >
  |}];
  return ()
;;
