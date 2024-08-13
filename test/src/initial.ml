open! Core
open! Async
open! Import
open Test_helpers

let%expect_test "initial selection" =
  test_blocking_and_async (fun arg ->
    let%bind () = test arg options [] in
    [%expect
      {|
        g
        f
        doodad
        d
        c
        b
      > a
        7/7
      >
      |}];
    return ())
;;

let%expect_test "initial selection with header" =
  test_blocking_and_async (fun arg ->
    let%bind () = test ~header:"FOO" arg options [] in
    [%expect
      {|
        g
        f
        doodad
        d
        c
        b
      > a
        FOO
        7/7
      >
      |}];
    return ())
;;

let%expect_test "initial selection with header and query" =
  test_blocking_and_async (fun arg ->
    let%bind () = test ~header:"FOO" ~query:"d" arg options [] in
    [%expect
      {|
        doodad
      > d
        FOO
        2/7
      > d
      |}];
    return ())
;;
