open! Core
open! Async
open! Import
open Test_helpers

let%expect_test _ =
  let%bind () =
    test_bash
      ~bash_cmd:(fun ~root -> root ^/ "lib/fzf/test/bin/example.exe from-command-output")
      [ Type "I typed this lowercase, the command uppercases it"; Enter ]
  in
  [%expect
    {|
    bash$ ROOT/lib/fzf/test/bin/example.exe from-command-output
    Picked: ("I TYPED THIS LOWERCASE, THE COMMAND UPPERCASES IT")
    bash$
    |}];
  return ()
;;
