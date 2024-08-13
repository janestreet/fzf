open! Core
open! Async
open! Import
open Test_helpers

let%expect_test "selection with only one match will prompt otherwise" =
  test_blocking_and_async (fun arg ->
    let%bind () = test ~header:"FOO" ~query:"dd" arg options [] in
    [%expect
      {|
      > doodad
        FOO
        1/7
      > dd
      |}];
    return ())
;;
