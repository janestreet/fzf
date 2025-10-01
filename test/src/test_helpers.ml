open! Core
open! Async
open! Import

module Tmux = struct
  open! Tmux

  let pause () = Clock.after (sec 1.0)

  let dump ~root tmux =
    let root = String.Search_pattern.create root in
    let blocking = String.Search_pattern.create "blocking" in
    let async = String.Search_pattern.create "async" in
    Tmux.dump_screen tmux
    >>| ok_exn
    >>| List.iter ~f:(fun line ->
      let line = String.Search_pattern.replace_all root ~in_:line ~with_:"ROOT" in
      let line =
        String.Search_pattern.replace_first blocking ~in_:line ~with_:"BLOCKING/ASYNC"
      in
      let line =
        String.Search_pattern.replace_first async ~in_:line ~with_:"BLOCKING/ASYNC"
      in
      print_endline line)
  ;;

  let send_chars tmux str = Tmux.send_chars tmux str >>| ok_exn >>= pause
  let send_keys tmux keys = Tmux.send_keys tmux keys >>| ok_exn >>= pause

  let bash ~width ~height f =
    Tmux.with_command
      ()
      ~size:{ Tmux.Size.width; height }
      ~command:"/bin/bash --rcfile <(echo \"PS1='bash$ '\")"
      ~f
    >>| Or_error.ok_exn
  ;;

  let write_command_autocompletion ~prompt ~exe ~rcfile =
    let%bind bash_lines =
      Process.run_lines
        ~prog:exe
        ~args:[]
        ~env:(`Extend [ "COMMAND_OUTPUT_INSTALLATION_BASH", "true" ])
        ()
      |> Deferred.Or_error.ok_exn
    in
    Writer.with_file rcfile ~f:(fun writer ->
      Writer.write_line writer [%string {|PS1='%{prompt} '|}];
      List.iter bash_lines ~f:(Writer.write_line writer);
      Writer.flushed writer)
  ;;

  let dump_until ?(debug = false) ?suppress_stdout ~complete_exe ~until tmux =
    let print_sanitized line =
      match suppress_stdout with
      | None ->
        String.substr_replace_all ~pattern:complete_exe ~with_:"EXE" line |> print_endline
      | Some () -> ()
    in
    let count = ref 0 in
    let matches lines =
      match until with
      | `Exact x ->
        String.( = ) (lines |> List.filter ~f:(Fn.non String.is_empty) |> List.last_exn) x
      | `Substring x -> List.exists ~f:(String.is_substring ~substring:x) lines
    in
    Deferred.repeat_until_finished () (fun () ->
      let%map lines = Tmux.dump_screen tmux |> Deferred.Or_error.ok_exn in
      if matches lines
      then `Finished lines
      else (
        incr count;
        if debug && !count > 1000
        then (
          print_endline "exceeded";
          `Finished lines)
        else `Repeat ()))
    >>| List.iter ~f:print_sanitized
  ;;

  let clear_prompt tmux = Tmux.send_keys tmux [ `Ctrl `u ] |> Deferred.Or_error.ok_exn
  let tab_complete tmux = send_keys tmux [ `Tab; `Tab ]

  let from_fresh_prompt_tab_complete ?debug ~prompt ~until ~complete_exe ~after_exe tmux =
    let%bind () = clear_prompt tmux in
    let%bind () =
      dump_until ~suppress_stdout:() ~until:(`Substring prompt) tmux ~complete_exe
    in
    let%bind () =
      Tmux.send_chars tmux (complete_exe ^ " " ^ after_exe) |> Deferred.Or_error.ok_exn
    in
    let%bind () = tab_complete tmux in
    dump_until ?debug ~until ~complete_exe tmux
  ;;
end

module Action = struct
  type t =
    | Type of string
    | Enter
    | Escape
    | Tab
    | Up
    | Control_a
    | Control_c
    | Key of Jane_term_types.Key.t
    | Wait of Time_ns.Span.t
end

let test_bash ?width ?(height = 10) ~bash_cmd actions =
  let%bind root =
    In_thread.run Jenga_rules_integration.blocking_root >>| Or_error.ok_exn
  in
  (* The default of 1200 is chosen to avoid wrapping in nearly all cases. We particularly
     don't want to wrap lines that reference unstable things like the root dir, we remove
     them via string substitution but they might cause wrapping to happen in different
     places despite that. *)
  let width = Option.value width ~default:1200 in
  Tmux.bash ~width ~height (fun tmux ->
    let bash_cmd = bash_cmd ~root in
    let%bind () = Tmux.send_chars tmux bash_cmd in
    let%bind () = Tmux.send_keys tmux [ `Enter ] in
    (* let%bind () = Tmux.dump "initial state" tmux in *)
    let%bind () =
      Deferred.List.iter ~how:`Sequential actions ~f:(fun action ->
        match (action : Action.t) with
        | Type chars -> Tmux.send_chars tmux chars
        | Enter -> Tmux.send_keys tmux [ `Enter ]
        | Escape -> Tmux.send_keys tmux [ `Esc ]
        | Tab -> Tmux.send_keys tmux [ `Tab ]
        | Up -> Tmux.send_keys tmux [ `Up ]
        | Control_a -> Tmux.send_keys tmux [ `Ctrl `a ]
        | Control_c -> Tmux.send_keys tmux [ `Ctrl `c ]
        | Key key -> Tmux.send_keys tmux [ key ]
        | Wait span -> Time_source.after (Time_source.wall_clock ()) span)
    in
    Tmux.dump ~root tmux)
;;

let option_to_command_argument ~argument value =
  Option.value ~default:"" (Option.map value ~f:(sprintf "-%s %s" argument))
;;

let flag_to_command_argument ~argument value =
  Option.value ~default:"" (Option.map value ~f:(fun () -> sprintf "-%s" argument))
;;

let test
  subcmd
  ?select1
  ?header
  ?query
  ?no_sort
  ?reverse_input
  ?prompt_at_top
  ?with_nth
  ?height
  ?bind
  ?tiebreak
  ?escaped
  ?case_match
  ?expect
  options
  actions
  =
  let options =
    match options with
    | [] -> "''"
    | options -> String.concat ~sep:"," options
  in
  let bash_cmd ~root =
    String.concat
      ~sep:" "
      [ root ^/ "/lib/fzf/test/bin/example.exe"
      ; subcmd
      ; options
      ; flag_to_command_argument ~argument:"select1" select1
      ; option_to_command_argument ~argument:"header" header
      ; option_to_command_argument ~argument:"query" query
      ; flag_to_command_argument ~argument:"no-sort" no_sort
      ; flag_to_command_argument ~argument:"reverse-input" reverse_input
      ; flag_to_command_argument ~argument:"prompt-at-top" prompt_at_top
      ; flag_to_command_argument ~argument:"escaped" escaped
      ; option_to_command_argument ~argument:"with-nth" with_nth
      ; option_to_command_argument ~argument:"height" (Option.map ~f:Int.to_string height)
      ; option_to_command_argument ~argument:"bind" bind
      ; option_to_command_argument ~argument:"tiebreak" tiebreak
      ; option_to_command_argument ~argument:"case-match" case_match
      ; option_to_command_argument ~argument:"expect" expect
      ]
  in
  test_bash ~bash_cmd actions
;;

let test_no_options subcmd actions =
  let bash_cmd ~root =
    String.concat ~sep:" " [ root ^/ "/lib/fzf/test/bin/example.exe"; subcmd ]
  in
  test_bash ~bash_cmd actions
;;

let options = [ "a"; "b"; "c"; "d"; "doodad"; "f"; "g" ]

let test_blocking_and_async f =
  Deferred.List.iter ~how:`Sequential [ "blocking"; "async" ] ~f
;;

let test_pick_many f = f "pick-many"
let test_pick_map f = f "from-map"
