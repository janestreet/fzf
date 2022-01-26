open! Core
open! Async

let connect_and_get_pipe hnp =
  let%bind client =
    Rpc.Connection.client (Tcp.Where_to_connect.of_host_and_port hnp)
    >>| Result.map_error ~f:Error.of_exn
    |> Deferred.Or_error.ok_exn
  in
  Rpc.Pipe_rpc.dispatch_exn Fzf_test_lib.pipe_rpc client () >>| fst
;;

let command =
  Command.async
    ~summary:"streaming fzf testing binary"
    (let%map_open.Command hnp = anon ("host and port" %: host_and_port) in
     fun () ->
       let%bind pipe = connect_and_get_pipe hnp in
       let%map result =
         Fzf.pick_one (Streaming (Fzf.Streaming.of_strings_raise_on_newlines pipe))
         |> Deferred.Or_error.ok_exn
       in
       print_s [%message (result : string option)])
;;

let () = Command_unix.run command
