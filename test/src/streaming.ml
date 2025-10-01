open! Core
open! Async
open! Import

let debug = ref false

let%expect_test "streaming" =
  let root = Jenga_rules_integration.blocking_root () |> ok_exn in
  let%bind () = Unix.chdir (root ^/ "lib/fzf/test/bin/") in
  let complete_exe = root ^/ "lib/fzf/test/bin/" ^ "streaming.exe" in
  let pipe_r, pipe_w = Pipe.create () in
  let%bind tcp_server =
    Rpc.Connection.serve
      ()
      ~initial_connection_state:(fun _ _ -> ())
      ~where_to_listen:Tcp.Where_to_listen.of_port_chosen_by_os
      ~implementations:
        (Rpc.Implementations.create_exn
           ~on_unknown_rpc:`Raise
           ~implementations:
             [ Rpc.Pipe_rpc.implement
                 Fzf_test_lib.pipe_rpc
                 (fun () () -> return (Ok pipe_r))
                 ~leave_open_on_exception:true
             ]
           ~on_exception:Log_on_background_exn)
  in
  let hnp =
    Host_and_port.create ~host:"127.0.0.1" ~port:(Tcp.Server.listening_on tcp_server)
  in
  let run_test tmux =
    let dump = Test_helpers.Tmux.dump_until ~debug:!debug tmux ~complete_exe in
    let%bind () = dump ~until:(`Substring "⠹ 0/0") in
    [%expect
      {|
      ⠹ 0/0
      >
      |}];
    let%bind () = Pipe.write pipe_w "test" in
    let%bind () = dump ~until:(`Substring "⠸ 1/1") in
    [%expect
      {|
      > test
      ⠸ 1/1
      >
      |}];
    let%bind () =
      List.init 30 ~f:Int.to_string
      |> List.map ~f:(Pipe.write pipe_w)
      |> Deferred.all_unit
    in
    let%bind () = dump ~until:(`Substring "⠼ 31/31") in
    [%expect
      {|
        6
        5
        4
        3
        2
        1
        0
      > test
      ⠼ 31/31
      >
      |}];
    Deferred.unit
  in
  Tmux.with_command
    ()
    ~size:{ Tmux.Size.width = 80; height = 10 }
    ~command:[%string {|%{complete_exe} %{Host_and_port.to_string hnp}|}]
    ~f:run_test
  |> Deferred.Or_error.ok_exn
;;

let%expect_test "streaming stringable" =
  let root = Jenga_rules_integration.blocking_root () |> ok_exn in
  let%bind () = Unix.chdir (root ^/ "lib/fzf/test/bin/") in
  let complete_exe = root ^/ "lib/fzf/test/bin/" ^ "streaming.exe" in
  let pipe_r, pipe_w = Pipe.create () in
  let%bind tcp_server =
    Rpc.Connection.serve
      ()
      ~initial_connection_state:(fun _ _ -> ())
      ~where_to_listen:Tcp.Where_to_listen.of_port_chosen_by_os
      ~implementations:
        (Rpc.Implementations.create_exn
           ~on_unknown_rpc:`Raise
           ~implementations:
             [ Rpc.Pipe_rpc.implement
                 Fzf_test_lib.pipe_rpc
                 (fun () () -> return (Ok pipe_r))
                 ~leave_open_on_exception:true
             ]
           ~on_exception:Log_on_background_exn)
  in
  let hnp =
    Host_and_port.create ~host:"127.0.0.1" ~port:(Tcp.Server.listening_on tcp_server)
  in
  let run_test tmux =
    let dump = Test_helpers.Tmux.dump_until ~debug:!debug tmux ~complete_exe in
    let%bind () = dump ~until:(`Substring "⠹ 0/0") in
    [%expect
      {|
      ⠹ 0/0
      >
      |}];
    let%bind () = Pipe.write pipe_w "(A 1)" in
    let%bind () = Pipe.write pipe_w "(B 2)" in
    let%bind () = Pipe.write pipe_w "(C 3)" in
    let%bind () = dump ~until:(`Substring "⠸ 3/3") in
    [%expect
      {|
        C
        B
      > A
      ⠸ 3/3
      >
      |}];
    let%bind () = Tmux.send_keys tmux [ `Up; `Enter ] |> Deferred.Or_error.ok_exn in
    let%bind () = dump ~until:(`Substring "result") in
    [%expect {| (result (2)) |}];
    Deferred.unit
  in
  Tmux.with_command
    ()
    ~size:{ Tmux.Size.width = 80; height = 10 }
    ~command:[%string {|%{complete_exe} %{Host_and_port.to_string hnp} -assoc|}]
    ~f:run_test
  |> Deferred.Or_error.ok_exn
;;

let%expect_test "streaming same string does not result in duplicate strings" =
  let root = Jenga_rules_integration.blocking_root () |> ok_exn in
  let%bind () = Unix.chdir (root ^/ "lib/fzf/test/bin/") in
  let complete_exe = root ^/ "lib/fzf/test/bin/" ^ "streaming.exe" in
  let pipe_r, pipe_w = Pipe.create () in
  let%bind tcp_server =
    Rpc.Connection.serve
      ()
      ~initial_connection_state:(fun _ _ -> ())
      ~where_to_listen:Tcp.Where_to_listen.of_port_chosen_by_os
      ~implementations:
        (Rpc.Implementations.create_exn
           ~on_unknown_rpc:`Raise
           ~implementations:
             [ Rpc.Pipe_rpc.implement
                 Fzf_test_lib.pipe_rpc
                 (fun () () -> return (Ok pipe_r))
                 ~leave_open_on_exception:true
             ]
           ~on_exception:Log_on_background_exn)
  in
  let hnp =
    Host_and_port.create ~host:"127.0.0.1" ~port:(Tcp.Server.listening_on tcp_server)
  in
  let run_test tmux =
    let dump = Test_helpers.Tmux.dump_until ~debug:!debug tmux ~complete_exe in
    let%bind () = dump ~until:(`Substring "⠹ 0/0") in
    [%expect
      {|
      ⠹ 0/0
      >
      |}];
    let%bind () = Pipe.write pipe_w "(A 1)" in
    let%bind () = Pipe.write pipe_w "(B 2)" in
    let%bind () = Pipe.write pipe_w "(A 1)" in
    let%bind () = dump ~until:(`Substring "⠸ 2/2") in
    [%expect
      {|
        B
      > A
      ⠸ 2/2
      >
      |}];
    let%bind () = Tmux.send_keys tmux [ `Enter ] |> Deferred.Or_error.ok_exn in
    let%bind () = dump ~until:(`Substring "result") in
    [%expect {| (result (1)) |}];
    Deferred.unit
  in
  Tmux.with_command
    ()
    ~size:{ Tmux.Size.width = 80; height = 10 }
    ~command:[%string {|%{complete_exe} %{Host_and_port.to_string hnp} -assoc|}]
    ~f:run_test
  |> Deferred.Or_error.ok_exn
;;

let%expect_test "streaming escaped is ok" =
  let root = Jenga_rules_integration.blocking_root () |> ok_exn in
  let%bind () = Unix.chdir (root ^/ "lib/fzf/test/bin/") in
  let complete_exe = root ^/ "lib/fzf/test/bin/" ^ "streaming.exe" in
  let pipe_r, pipe_w = Pipe.create () in
  let%bind tcp_server =
    Rpc.Connection.serve
      ()
      ~initial_connection_state:(fun _ _ -> ())
      ~where_to_listen:Tcp.Where_to_listen.of_port_chosen_by_os
      ~implementations:
        (Rpc.Implementations.create_exn
           ~on_unknown_rpc:`Raise
           ~implementations:
             [ Rpc.Pipe_rpc.implement
                 Fzf_test_lib.pipe_rpc
                 (fun () () -> return (Ok pipe_r))
                 ~leave_open_on_exception:true
             ]
           ~on_exception:Log_on_background_exn)
  in
  let hnp =
    Host_and_port.create ~host:"127.0.0.1" ~port:(Tcp.Server.listening_on tcp_server)
  in
  let run_test tmux =
    let dump = Test_helpers.Tmux.dump_until ~debug:!debug tmux ~complete_exe in
    let%bind () = dump ~until:(`Substring "⠹ 0/0") in
    [%expect
      {|
      ⠹ 0/0
      >
      |}];
    let%bind () = Pipe.write pipe_w "(\"a\\/b\\/c\" 1)" in
    let%bind () = dump ~until:(`Substring "⠸ 1/1") in
    [%expect
      {|
      > a\\/b\\/c
      ⠸ 1/1
      >
      |}];
    let%bind () = Tmux.send_keys tmux [ `Enter ] |> Deferred.Or_error.ok_exn in
    let%bind () = dump ~until:(`Substring "result") in
    [%expect {| (result (1)) |}];
    Deferred.unit
  in
  Tmux.with_command
    ()
    ~size:{ Tmux.Size.width = 80; height = 10 }
    ~command:[%string {|%{complete_exe} %{Host_and_port.to_string hnp} -assoc|}]
    ~f:run_test
  |> Deferred.Or_error.ok_exn
;;

let%expect_test "demonstrate streaming works" =
  let%bind () =
    Test_helpers.test_no_options "streaming" [ Wait (Time_ns.Span.of_sec 2.); Enter ]
  in
  [%expect
    {|
    bash$ ROOT/lib/fzf/test/bin/example.exe streaming
    Picked: (0)
    bash$
    |}];
  return ()
;;
