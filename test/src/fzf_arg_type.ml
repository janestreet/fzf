open! Core
open! Async
open! Import

let debug = ref false
let prompt = "bash$"

let%expect_test "fzf completion" =
  let root = Jenga_rules_integration.blocking_root () |> ok_exn in
  let%bind () = Unix.chdir (root ^/ "lib/fzf/test/bin/") in
  let complete_exe = "./arg_type_complete_example.exe" in
  let run_test tmux =
    let dump = Test_helpers.Tmux.dump_until ~debug:!debug tmux ~complete_exe in
    let from_fresh_prompt_tab_complete ?(until = `Exact ">") ~after_exe () =
      Test_helpers.Tmux.from_fresh_prompt_tab_complete
        ~debug:!debug
        ~prompt
        ~until
        ~complete_exe
        ~after_exe
        tmux
    in
    let%bind () = from_fresh_prompt_tab_complete ~after_exe:"-egg-choice " () in
    [%expect
      {|
        sunny-side-up
        scrambled
      > poached
        3/3
      >
      |}];
    let%bind () = Tmux.send_keys tmux [ `Up; `Enter ] |> Deferred.Or_error.ok_exn in
    let%bind () = dump ~until:(`Substring "-egg-choice scrambled") in
    [%expect {| bash$ EXE -egg-choice scrambled |}];
    let%bind () =
      from_fresh_prompt_tab_complete ~until:(`Exact "> po") ~after_exe:"-egg-choice po" ()
    in
    [%expect
      {|
      > poached
        1/3
      > po
      |}];
    let%bind () = Tmux.send_keys tmux [ `Enter ] |> Deferred.Or_error.ok_exn in
    let%bind () = dump ~until:(`Substring "-egg-choice poached") in
    [%expect {| bash$ EXE -egg-choice poached |}];
    let%bind () = from_fresh_prompt_tab_complete ~after_exe:"-egg-choice" () in
    [%expect
      {|
        sunny-side-up
        scrambled
      > poached
        3/3
      >
      |}];
    let%bind () = Tmux.send_keys tmux [ `Char 's' ] |> Deferred.Or_error.ok_exn in
    let%bind () = Clock.after (sec 1.0) in
    let%bind () = dump ~until:(`Exact "> s") in
    [%expect
      {|
        sunny-side-up
      > scrambled
        2/3
      > s
      |}];
    let%bind () = Tmux.send_keys tmux [ `Enter ] |> Deferred.Or_error.ok_exn in
    let%bind () = dump ~until:(`Substring "-egg-choice scrambled") in
    [%expect {| bash$ EXE -egg-choice scrambled |}];
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
