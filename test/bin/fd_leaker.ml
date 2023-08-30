open! Core
open! Async
open Deferred.Or_error.Let_syntax

let get_open_pipes () =
  Process.run_lines
    ~prog:"/bin/bash"
    ~args:
      [ "-c"
      ; [%string "/usr/bin/lsof -p %{Unix.getpid () |> Pid.to_string} | grep pipe"]
      ]
    ()
  >>| (List.length :> _ -> _)
;;

let command =
  Command.async_or_error
    ~summary:""
    (let%map_open.Command () = return () in
     fun () ->
       let iterations = 5 in
       let%bind baseline = get_open_pipes () in
       let%bind () =
         Deferred.Or_error.List.iter
           ~how:`Sequential
           (List.init iterations ~f:(Fn.const ()))
           ~f:(fun () ->
             let%bind () =
               match%bind
                 Fzf.pick_one_abort
                   ~abort:Deferred.unit
                   (Fzf.Pick_from.Inputs [ "a"; "b" ])
               with
               | Second `Aborted -> return ()
               | First _ -> failwith "unexpected result"
             in
             let%bind open_pipes = get_open_pipes () in
             printf "%d above baseline\n" (open_pipes - baseline);
             return ())
       in
       return ())
    ~behave_nicely_in_pipeline:false
;;

let () = Command_unix.run command
