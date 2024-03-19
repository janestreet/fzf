open! Core
open! Async
open! Import

let run () =
  let root = Jenga_rules_integration.blocking_root () |> ok_exn in
  let prog = root ^/ "lib/fzf/test/bin/fd_leaker.exe" in
  Process.run ~prog ~args:[] ()
;;

let%expect_test _ =
  let%bind () = run () >>| ok_exn >>| print_endline in
  [%expect
    {|
    0 above baseline
    0 above baseline
    0 above baseline
    0 above baseline
    0 above baseline
    |}];
  return ()
;;
