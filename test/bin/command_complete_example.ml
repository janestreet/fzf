open! Core
open! Async

let command =
  Command.basic
    ~summary:"Example command with nested groups"
    (let%map_open.Command () = return () in
     fun () -> Core.print_endline "Goodbye, cruel world!")
;;

let command =
  let depth_list = List.init ~f:Fn.id 4 in
  List.fold_right depth_list ~init:command ~f:(fun idx acc ->
    Command.group ~summary:"summary" [ sprintf "%dsubcommand" idx, acc ])
;;

let () =
  Command_unix.run
    ~complete_subcommands:(Fzf.complete_subcommands ~show_help:true)
    command
;;
