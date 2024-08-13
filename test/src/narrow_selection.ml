open! Core
open! Async
open! Import
open Test_helpers

let%expect_test "type a second letter" =
  test_blocking_and_async (fun arg ->
    let%bind () = test arg options [ Type "do" ] in
    [%expect
      {|
      > doodad
        1/7
      > do
      |}];
    return ())
;;

let%expect_test "final do selection" =
  test_blocking_and_async (fun arg ->
    let%bind () = test arg options [ Type "do"; Enter ] in
    [%expect
      {|
      bash$ ROOT/lib/fzf/test/bin/example.exe BLOCKING/ASYNC a,b,c,d,doodad,f,g
      Picked: (doodad)
      bash$
      |}];
    return ())
;;
