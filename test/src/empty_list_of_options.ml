open! Core
open! Async
open! Import
open Test_helpers

let%expect_test "don't crash pick_one on empty list of options" =
  test_blocking_and_async (fun arg ->
    let%bind () = test ~header:"FOO" ~query:"dd" arg [] [] in
    [%expect
      {|
      bash$ ROOT/lib/fzf/test/bin/example.exe BLOCKING/ASYNC ''  -header FOO -query dd
      Picked: ()
      bash$
      |}];
    return ())
;;
