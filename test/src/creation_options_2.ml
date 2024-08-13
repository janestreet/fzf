open! Core
open! Async
open! Import
open Test_helpers

(* More creation options tests, split up into a separate file so creation_options.ml
   doesn't timeout. *)

let%expect_test "tiebreak" =
  test_blocking_and_async (fun arg ->
    let%bind () = test ~tiebreak:"begin" arg [ "a.b"; "b.a" ] [ Type "a" ] in
    [%expect
      {|
        b.a
      > a.b
        2/2
      > a
      |}];
    let%bind () = test ~tiebreak:"end" arg [ "a.b"; "b.a" ] [ Type "a" ] in
    [%expect
      {|
        a.b
      > b.a
        2/2
      > a
      |}];
    return ())
;;
