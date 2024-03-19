open! Core
open! Async
open! Import
open Test_helpers

let%expect_test "String with a newline is one of the keys of a map" =
  test_pick_map (fun arg ->
    let%bind () = test arg [ "$'h\ni'" ] [ Enter ] in
    [%expect
      {|
      bash$ ROOT/lib/fzf/test/bin/example.exe from-map $'h
      > i'
      Picked: (One)
      bash$
      |}];
    return ())
;;

let%expect_test "Other options that require escaping are correctly handled when one \
                 option contains a newline"
  =
  test_pick_map (fun arg ->
    let%bind () = test arg ~escaped:() [ "\\n"; "\\000" ] [ Enter ] in
    [%expect
      {|
      bash$ ROOT/lib/fzf/test/bin/example.exe from-map \n,\000       -escaped
      Picked: (Two)
      bash$
      |}];
    return ())
;;
