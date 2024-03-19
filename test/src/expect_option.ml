open! Core
open! Async
open! Import
open Test_helpers

let%expect_test "pick one with expect" =
  test_pick_map (fun arg ->
    let%bind () = test ~expect:"ctrl-s" arg options [ Up; Key (`Ctrl `s) ] in
    [%expect
      {|
      bash$ ROOT/lib/fzf/test/bin/example.exe from-map a,b,c,d,doodad,f,g             -expect ctrl-s
      Picked: (Two)
      Key pressed: ctrl-s
      bash$
      |}];
    return ())
;;

let%expect_test "use enter despite expect" =
  test_pick_map (fun arg ->
    (* Since "enter" is not specified in "expect", fzf returns an empty string*)
    let%bind () = test ~expect:"ctrl-s" arg options [ Up; Enter ] in
    [%expect
      {|
      bash$ ROOT/lib/fzf/test/bin/example.exe from-map a,b,c,d,doodad,f,g             -expect ctrl-s
      Picked: (Two)
      Key pressed:
      bash$
      |}];
    return ())
;;

let%expect_test "pick one enter" =
  test_pick_map (fun arg ->
    let%bind () = test ~expect:"enter,ctrl-s" arg options [ Up; Enter ] in
    [%expect
      {|
      bash$ ROOT/lib/fzf/test/bin/example.exe from-map a,b,c,d,doodad,f,g             -expect enter,ctrl-s
      Picked: (Two)
      Key pressed: enter
      bash$
      |}];
    return ())
;;

let%expect_test "pick many with expect" =
  test_pick_many (fun arg ->
    let%bind () = test ~expect:"ctrl-s" arg options [ Up; Tab; Tab; Key (`Ctrl `s) ] in
    [%expect
      {|
      bash$ ROOT/lib/fzf/test/bin/example.exe pick-many a,b,c,d,doodad,f,g             -expect ctrl-s
      Picked: ("(b a)")
      Key pressed: ctrl-s
      bash$
      |}];
    return ())
;;

let%expect_test "pick many enter despite expect" =
  test_pick_many (fun arg ->
    let%bind () = test ~expect:"ctrl-s" arg options [ Up; Tab; Tab; Enter ] in
    [%expect
      {|
      bash$ ROOT/lib/fzf/test/bin/example.exe pick-many a,b,c,d,doodad,f,g             -expect ctrl-s
      Picked: ("(b a)")
      Key pressed:
      bash$
      |}];
    return ())
;;
