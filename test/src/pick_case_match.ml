open! Core
open! Async
open! Import
open Test_helpers

let options = [ "bar"; "baR"; "BAR"; "BAr" ]
let test arg query ~case_match = test arg options [ Type query ] ~case_match

let%expect_test "pick smart-case match" =
  test_blocking_and_async (fun arg ->
    let run query = test arg query ~case_match:"smart_case" in
    let%bind () = run "bar" in
    [%expect
      {|
        BAr
        BAR
        baR
      > bar
        4/4
      > bar
      |}];
    let%bind () = run "baR" in
    [%expect
      {|
      > baR
        1/4
      > baR
      |}];
    let%bind () = run "bAR" in
    [%expect
      {|
        0/4
      > bAR
      |}];
    return ())
;;

let%expect_test "pick case-sensitive match" =
  test_blocking_and_async (fun arg ->
    let test query = test arg query ~case_match:"case_sensitive" in
    let%bind () = test "bar" in
    [%expect
      {|
      > bar
        1/4
      > bar
      |}];
    let%bind () = test "baR" in
    [%expect
      {|
      > baR
        1/4
      > baR
      |}];
    let%bind () = test "bAR" in
    [%expect
      {|
        0/4
      > bAR
      |}];
    return ())
;;

let%expect_test "pick case-insensitive match " =
  test_blocking_and_async (fun arg ->
    let run query = test arg query ~case_match:"case_insensitive" in
    let%bind () = run "bar" in
    [%expect
      {|
        BAr
        BAR
        baR
      > bar
        4/4
      > bar
      |}];
    let%bind () = run "baR" in
    [%expect
      {|
        BAr
        BAR
        baR
      > bar
        4/4
      > baR
      |}];
    let%bind () = run "bAR" in
    [%expect
      {|
        BAr
        BAR
        baR
      > bar
        4/4
      > bAR
      |}];
    return ())
;;
