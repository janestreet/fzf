open! Core
open! Async
open! Import
open Expect_test_helpers_core
open Expect_test_helpers_async
open Test_helpers

let test ~args =
  let fzf = "fzf" in
  with_temp_dir (fun tempdir ->
    let%bind () =
      Writer.save
        ~perm:0o755
        (tempdir ^/ "fzf-wrapper-ok.sh")
        ~contents:[%string "#!/bin/bash\n%{fzf} --tac\n"]
    in
    let%bind () =
      Writer.save
        (tempdir ^/ "fzf-wrapper-not-executable.sh")
        ~contents:[%string "#!/bin/bash\n%{fzf} --tac\n"]
    in
    let%bind () =
      Writer.save
        (tempdir ^/ "fzf-wrapper-bad-interpreter.sh")
        ~perm:0o755
        ~contents:[%string "#!/bin/nonexistent-interpreter\n%{fzf} --tac\n"]
    in
    let args = args ~tempdir in
    let%bind () =
      test_bash
        ~bash_cmd:(fun ~root ->
          let prog = root ^/ "lib/fzf/test/bin/at_exit_handlers.exe" in
          [%string "OCAMLRUNPARAM=b=0 %{prog} %{args}"])
        [ Enter ]
    in
    print_endline (replace (expect_test_output ()) ~pattern:tempdir ~with_:"TEMP");
    return ())
;;

let%expect_test "fzf exists" =
  let%bind () = test ~args:(fun ~tempdir:_ -> "") in
  [%expect
    {|
    bash$ OCAMLRUNPARAM=b=0 ROOT/lib/fzf/test/bin/at_exit_handlers.exe
    (response (a))
    dummy at_exit handler
    bash$
    |}];
  return ()
;;

let%expect_test "fzf wrapper script is executable" =
  let%bind () =
    test ~args:(fun ~tempdir -> "-fzf-path " ^ tempdir ^/ "fzf-wrapper-ok.sh")
  in
  [%expect
    {|
    bash$ OCAMLRUNPARAM=b=0 ROOT/lib/fzf/test/bin/at_exit_handlers.exe -fzf-path TEMP/fzf-wrapper-ok.sh
    (response (c))
    dummy at_exit handler
    bash$
    |}];
  return ()
;;

let%expect_test "fzf doesn't exist" =
  let%bind () =
    test ~args:(fun ~tempdir:_ -> "-fzf-path /dummy-binary-that-doesnt-exist")
  in
  [%expect
    {|
    bash$ OCAMLRUNPARAM=b=0 ROOT/lib/fzf/test/bin/at_exit_handlers.exe -fzf-path /dummy-binary-that-doesnt-exist
    dummy at_exit handler
    Uncaught exception:

      (Unix.Unix_error "No such file or directory" "Core_unix.fork_exec: exec"
       /dummy-binary-that-doesnt-exist)

    bash$
    bash$
    |}];
  return ()
;;

let%expect_test "fzf wrapper script not executable" =
  let%bind () =
    test ~args:(fun ~tempdir -> "-fzf-path " ^ tempdir ^/ "fzf-wrapper-not-executable.sh")
  in
  [%expect
    {|
    bash$ OCAMLRUNPARAM=b=0 ROOT/lib/fzf/test/bin/at_exit_handlers.exe -fzf-path TEMP/fzf-wrapper-not-executable.sh
    dummy at_exit handler
    Uncaught exception:

      (Unix.Unix_error "Permission denied" "Core_unix.fork_exec: exec"
       TEMP/fzf-wrapper-not-executable.sh)

    bash$
    bash$
    |}];
  return ()
;;

let%expect_test "fzf wrapper script has bad interpreter" =
  let%bind () =
    test ~args:(fun ~tempdir ->
      "-fzf-path " ^ tempdir ^/ "fzf-wrapper-bad-interpreter.sh")
  in
  [%expect
    {|
    bash$ OCAMLRUNPARAM=b=0 ROOT/lib/fzf/test/bin/at_exit_handlers.exe -fzf-path TEMP/fzf-wrapper-bad-interpreter.sh
    dummy at_exit handler
    Uncaught exception:

      (Unix.Unix_error "No such file or directory" "Core_unix.fork_exec: exec"
       TEMP/fzf-wrapper-bad-interpreter.sh)

    bash$
    bash$
    |}];
  return ()
;;
