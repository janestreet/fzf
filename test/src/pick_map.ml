open! Core
open! Async
open! Import
open Test_helpers

let%expect_test "Pick from map" =
  test_pick_map (fun arg ->
    let%bind () = test arg options [ Enter ] in
    [%expect
      {|
      bash$ ROOT/lib/fzf/test/bin/example.exe from-map a,b,c,d,doodad,f,g
      Picked: (One)
      bash$
      |}];
    return ())
;;
