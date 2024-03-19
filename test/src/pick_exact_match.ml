open! Core
open! Async
open! Import
open Test_helpers

let%expect_test "final d selection" =
  test_blocking_and_async (fun arg ->
    let%bind () = test arg options [ Type "d"; Enter ] in
    [%expect
      {|
      bash$ ROOT/lib/fzf/test/bin/example.exe BLOCKING/ASYNC a,b,c,d,doodad,f,g
      Picked: (d)
      bash$
      |}];
    return ())
;;
