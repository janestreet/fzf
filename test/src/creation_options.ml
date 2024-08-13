open! Core
open! Async
open! Import
open Test_helpers

let%expect_test "reverse input" =
  test_blocking_and_async (fun arg ->
    let%bind () = test ~reverse_input:() arg options [] in
    [%expect
      {|
        a
        b
        c
        d
        doodad
        f
      > g
        7/7
      >
      |}];
    return ())
;;

let%expect_test "prompt at top" =
  test_blocking_and_async (fun arg ->
    let%bind () = test ~prompt_at_top:() arg options [] in
    [%expect
      {|
      >
        7/7
      > a
        b
        c
        d
        doodad
        f
        g
      |}];
    return ())
;;

let%expect_test "prompt at top, reversed input with header" =
  test_blocking_and_async (fun arg ->
    let%bind () = test ~reverse_input:() ~prompt_at_top:() ~header:"FOO" arg options [] in
    [%expect
      {|
      >
        7/7
        FOO
      > g
        f
        doodad
        d
        c
        b
        a
      |}];
    return ())
;;

let%expect_test "with-nth" =
  test_blocking_and_async (fun arg ->
    let%bind () =
      test ~with_nth:"2.." arg [ "'x1 x2'"; "'y1 y2 y3'"; "'z1 z2 z3 z4'" ] []
    in
    [%expect
      {|
        z2 z3 z4
        y2 y3
      > x2
        3/3
      >
      |}];
    return ())
;;

let%expect_test "height" =
  test_blocking_and_async (fun arg ->
    let%bind () = test ~height:5 arg [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] [] in
    [%expect
      {|
      bash$ ROOT/lib/fzf/test/bin/example.exe BLOCKING/ASYNC a,b,c,d,e,f,g,h         -height 5
        c
        b
      > a
        8/8
      >
      |}];
    return ())
;;

let%expect_test "height + scroll to top" =
  test_blocking_and_async (fun arg ->
    let%bind () = test ~height:5 arg [ "a"; "b"; "c"; "d" ] [ Up; Up; Up; Up ] in
    [%expect
      {|
      bash$ ROOT/lib/fzf/test/bin/example.exe BLOCKING/ASYNC a,b,c,d         -height 5
      > d
        c
        b
        4/4
      >
      |}];
    return ())
;;

let%expect_test "custom key bindings, never accept" =
  test_blocking_and_async (fun arg ->
    let%bind () = test ~bind:"enter:ignore" arg [ "a"; "b"; "c"; "d" ] [ Enter ] in
    [%expect
      {|
        d
        c
        b
      > a
        4/4
      >
      |}];
    return ())
;;

let%expect_test "custom key bindings, accept with TAB" =
  test_blocking_and_async (fun arg ->
    let%bind () =
      test ~bind:"enter:ignore,tab:accept" arg [ "a"; "b"; "c"; "d" ] [ Enter; Tab ]
    in
    [%expect
      {|
      bash$ ROOT/lib/fzf/test/bin/example.exe BLOCKING/ASYNC a,b,c,d          -bind enter:ignore,tab:accept
      Picked: (a)
      bash$
      |}];
    return ())
;;
