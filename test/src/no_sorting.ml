open! Core
open! Async
open! Import
open Test_helpers

let%expect_test "type a letter, no sorting" =
  test_blocking_and_async (fun arg ->
    let%bind () = test ~no_sort:() arg options [ Type "d" ] in
    [%expect
      {|
        doodad
      > d
        2/7
      > d
      |}];
    return ())
;;
