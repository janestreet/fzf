open! Core
open! Async
open! Import
open Test_helpers

let%expect_test "Type then select 2 entries and hit enter" =
  test_pick_many (fun arg ->
    let%bind () = test arg options [ Type "d"; Up; Tab; Tab; Enter ] in
    [%expect
      {|
      bash$ ROOT/lib/fzf/test/bin/example.exe pick-many a,b,c,d,doodad,f,g
      Picked: ("(doodad d)")
      bash$
      |}];
    return ())
;;

let%expect_test "Select all entries and hit enter" =
  test_pick_many (fun arg ->
    let%bind () = test arg options [ Control_a; Enter ] in
    [%expect
      {|
      bash$ ROOT/lib/fzf/test/bin/example.exe pick-many a,b,c,d,doodad,f,g
      Picked: ("(a b c d doodad f g)")
      bash$
      |}];
    return ())
;;

let%expect_test "selection with only one match will not prompt if we pass [select1]" =
  test_blocking_and_async (fun arg ->
    let%bind () = test ~header:"FOO" ~query:"dd" ~select1:() arg options [] in
    [%expect
      {|
      bash$ ROOT/lib/fzf/test/bin/example.exe BLOCKING/ASYNC a,b,c,d,doodad,f,g -select1 -header FOO -query dd
      Picked: (doodad)
      bash$
      |}];
    return ())
;;
