open! Core

let () = at_exit (fun () -> prerr_endline "dummy at_exit handler")

let command =
  Command.basic
    ~summary:"example command using fzf with at_exit handlers"
    (let%map_open.Command () = return ()
     and fzf_path = flag "-fzf-path" (optional string) ~doc:"PATH path to fzf binary" in
     fun () ->
       let response = Fzf.Blocking.pick_one ?fzf_path (Inputs [ "a"; "b"; "c" ]) in
       print_s [%message (response : string option)])
;;

let () = Command_unix.run command
