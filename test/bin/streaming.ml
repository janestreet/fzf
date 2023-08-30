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
    (let%map_open.Command hnp = anon ("host and port" %: host_and_port)
     and assoc =
       flag
         "assoc"
         no_arg
         ~doc:
           " Read association pairs, line-by-line, and use \
            [Streaming.of_escaped_strings_assoc]."
     in
     fun () ->
       let%bind pipe = connect_and_get_pipe hnp in
       let streaming =
         match assoc with
         | false -> Fzf.Streaming.of_strings_raise_on_newlines pipe
         | true ->
           Fzf.Streaming.of_escaped_strings_assoc
             (Pipe.map pipe ~f:(fun elt ->
                [%of_sexp: string * string] (Sexp.of_string elt)))
             ~on_collision:(fun ~old_item ~new_item ->
               match [%equal: string] old_item new_item with
               | true -> `Ignore
               | false -> `Raise (Error.create_s [%message "Collision!"]))
       in
       let%map result = Fzf.pick_one (Streaming streaming) |> Deferred.Or_error.ok_exn in
       print_s [%message (result : string option)])
    ~behave_nicely_in_pipeline:false
;;

let () = Command_unix.run command
