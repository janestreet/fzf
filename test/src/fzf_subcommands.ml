open! Core
open! Async
open! Import

let prompt = "bash$"
let debug = ref false
let last_subcommand_description = "print this help"

let%expect_test "fzf completion" =
  let root = Jenga_rules_integration.blocking_root () |> ok_exn in
  let%bind () = Unix.chdir (root ^/ "lib/fzf/test/bin/") in
  let complete_exe = "./command_complete_example.exe" in
  let run_test tmux =
    let dump = Test_helpers.Tmux.dump_until ~debug:!debug tmux ~complete_exe in
    let from_fresh_prompt_tab_complete
      ?(until = `Substring last_subcommand_description)
      ~after_exe
      ()
      =
      Test_helpers.Tmux.from_fresh_prompt_tab_complete
        ~debug:!debug
        ~prompt
        ~until
        ~complete_exe
        ~after_exe
        tmux
    in
    let%bind () = from_fresh_prompt_tab_complete ~after_exe:"" () in
    [%expect
      {|
      >                                                                          ╭─────────────────────────────────────────────────────────────────────────╮
        6/6                                                                      │ Example command with nested groups                                      │
      > 0subcommand 1subcommand 2subcommand 3subcommand                          │                                                                         │
        0subcommand 1subcommand 2subcommand help                                 │   command_complete_example.exe 0subcommand 1subcommand 2subcommand 3sub │
        0subcommand 1subcommand help                                             │                                                                         │
        0subcommand help                                                         │ === flags ===                                                           │
        version                                                                  │                                                                         │
        help                                                                     │   [-help], -?                . print this help text and exit            │
                                                                                 │                                                                         │
                                                                                 │                                                                         │
                                                                                 │                                                                         │
                                                                                 │                                                                         │
                                                                                 │                                                                         │
                                                                                 │                                                                         │
                                                                                 │                                                                         │
                                                                                 │                                                                         │
                                                                                 │                                                                         │
                                                                                 │                                                                         │
                                                                                 │                                                                         │
                                                                                 │                                                                         │
                                                                                 │                                                                         │
                                                                                 │                                                                         │
                                                                                 │                                                                         │
                                                                                 │                                                                         │
                                                                                 │                                                                         │
                                                                                 │                                                                         │
                                                                                 │                                                                         │
                                                                                 │                                                                         │
                                                                                 │                                                                         │
                                                                                 ╰─────────────────────────────────────────────────────────────────────────╯
      |}];
    let%bind () =
      Tmux.send_keys tmux [ `Down; `Down; `Down; `Enter ] |> Deferred.Or_error.ok_exn
    in
    let%bind () = dump ~until:(`Substring "0subcommand help") in
    [%expect {| bash$ EXE 0subcommand help |}];
    let%bind () = from_fresh_prompt_tab_complete ~after_exe:"0subcommand 1" () in
    [%expect
      {|
      > 1                                                                        ╭─────────────────────────────────────────────────────────────────────────╮
        3/4                                                                      │ explain a given subcommand (perhaps recursively)                        │
      > 1subcommand help                                                         │                                                                         │
        1subcommand 2subcommand help                                             │   command_complete_example.exe 0subcommand 1subcommand help [SUBCOMMAND │
        1subcommand 2subcommand 3subcommand                                      │                                                                         │
                                                                                 │ === flags ===                                                           │
                                                                                 │                                                                         │
                                                                                 │   [-expand-dots]             . expand subcommands in recursive help     │
                                                                                 │   [-flags]                   . show flags as well in recursive help     │
                                                                                 │   [-recursive]               . show subcommands of subcommands, etc.    │
                                                                                 │   [-help], -?                . print this help text and exit            │
                                                                                 │                                                                         │
                                                                                 │                                                                         │
                                                                                 │                                                                         │
                                                                                 │                                                                         │
                                                                                 │                                                                         │
                                                                                 │                                                                         │
                                                                                 │                                                                         │
                                                                                 │                                                                         │
                                                                                 │                                                                         │
                                                                                 │                                                                         │
                                                                                 │                                                                         │
                                                                                 │                                                                         │
                                                                                 │                                                                         │
                                                                                 │                                                                         │
                                                                                 │                                                                         │
                                                                                 │                                                                         │
                                                                                 │                                                                         │
                                                                                 │                                                                         │
                                                                                 ╰─────────────────────────────────────────────────────────────────────────╯
      |}];
    let%bind () =
      Tmux.send_keys tmux [ `Down; `Down; `Enter ] |> Deferred.Or_error.ok_exn
    in
    let%bind () =
      dump ~until:(`Substring "0subcommand 1subcommand 2subcommand 3subcommand")
    in
    [%expect {| bash$ EXE 0subcommand 1subcommand 2subcommand 3subcommand |}];
    let%bind () = from_fresh_prompt_tab_complete ~after_exe:"0subcommand 1sub " () in
    [%expect
      {|
      >                                                                          ╭─────────────────────────────────────────────────────────────────────────╮
        3/3                                                                      │ Example command with nested groups                                      │
      > 2subcommand 3subcommand                                                  │                                                                         │
        2subcommand help                                                         │   command_complete_example.exe 0subcommand 1subcommand 2subcommand 3sub │
        help                                                                     │                                                                         │
                                                                                 │ === flags ===                                                           │
                                                                                 │                                                                         │
                                                                                 │   [-help], -?                . print this help text and exit            │
                                                                                 │                                                                         │
                                                                                 │                                                                         │
                                                                                 │                                                                         │
                                                                                 │                                                                         │
                                                                                 │                                                                         │
                                                                                 │                                                                         │
                                                                                 │                                                                         │
                                                                                 │                                                                         │
                                                                                 │                                                                         │
                                                                                 │                                                                         │
                                                                                 │                                                                         │
                                                                                 │                                                                         │
                                                                                 │                                                                         │
                                                                                 │                                                                         │
                                                                                 │                                                                         │
                                                                                 │                                                                         │
                                                                                 │                                                                         │
                                                                                 │                                                                         │
                                                                                 │                                                                         │
                                                                                 │                                                                         │
                                                                                 │                                                                         │
                                                                                 ╰─────────────────────────────────────────────────────────────────────────╯
      |}];
    let%bind () = Tmux.send_keys tmux [ `Down; `Enter ] |> Deferred.Or_error.ok_exn in
    let%bind () = dump ~until:(`Substring "2subcommand help") in
    [%expect {| bash$ EXE 0subcommand 1sub 2subcommand help |}];
    Deferred.unit
  in
  let rcfile = Filename_unix.temp_file "rcfile" "bash" in
  let%bind () =
    Test_helpers.Tmux.write_command_autocompletion ~prompt ~exe:complete_exe ~rcfile
  in
  Tmux.with_command
    ()
    ~size:{ Tmux.Size.width = 150; height = 30 }
    ~command:[%string {|/bin/bash --rcfile %{rcfile}|}]
    ~f:run_test
  |> Deferred.Or_error.ok_exn
;;
