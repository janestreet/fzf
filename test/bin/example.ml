open! Core

type all_options =
  { header : string option
  ; query : string option
  ; select1 : unit option
  ; no_sort : unit option
  ; reverse_input : unit option
  ; prompt_at_top : unit option
  ; with_nth : string option
  ; height : int option
  ; bind : string Nonempty_list.t option
  ; tiebreak : Fzf.Tiebreak.t Nonempty_list.t option
  ; case_match : [ `case_insensitive | `case_sensitive | `smart_case ] option
  ; expect : Fzf.Expect.t option
  }

let key_pressed = Set_once.create ()

let options_param =
  let open Command.Let_syntax in
  [%map_open
    let options =
      anon ("options" %: Command.Arg_type.comma_separated string ~allow_empty:true)
    and escaped =
      flag "escaped" no_arg ~doc:"options should be interpreted as escaped strings"
    in
    if escaped then List.map ~f:Scanf.unescaped options else options]
;;

let param =
  let%map_open.Command query =
    flag
      "query"
      (optional string)
      ~doc:"STRING Apply this initial query filter to the input options"
  and header =
    flag "header" (optional string) ~doc:"STRING Include this header in the output"
  and select1 =
    flag "select1" no_arg ~doc:"If only one option exists, select it immediately"
    |> map ~f:(function
      | true -> Some ()
      | false -> None)
  and no_sort =
    flag "no-sort" no_arg ~doc:"Don't sort the input when filtering"
    |> map ~f:(function
      | true -> Some ()
      | false -> None)
  and reverse_input =
    flag
      "reverse-input"
      no_arg
      ~doc:"Make the top line of the input options be furthest from the prompt"
    |> map ~f:(function
      | true -> Some ()
      | false -> None)
  and prompt_at_top =
    flag "prompt-at-top" no_arg ~doc:"Put the prompt at the top of the window"
    |> map ~f:(function
      | true -> Some ()
      | false -> None)
  and with_nth =
    flag
      "with-nth"
      (optional string)
      ~doc:
        "STRING show only part of every item, e.g. 2.. means show only 2nd field onwards"
  and height =
    flag
      "height"
      (optional int)
      ~doc:
        "INT fzf will display window with this height, in lines, instead of a full screen"
  and bind =
    flag
      "bind"
      (optional (Arg_type.comma_separated string))
      ~doc:"STRING custom key bindings (see man fzf)"
    |> map ~f:(Option.map ~f:Nonempty_list.of_list_exn)
  and tiebreak =
    flag
      "tiebreak"
      (optional Arg_type.(comma_separated (create Fzf.Tiebreak.of_string)))
      ~doc:"STRING comma-separated tiebreaks (see man fzf)"
    |> map ~f:(Option.map ~f:Nonempty_list.of_list_exn)
  and case_match =
    flag
      "case-match"
      (optional
         (Arg_type.create
            (Fn.compose
               [%of_sexp: [ `case_insensitive | `case_sensitive | `smart_case ]]
               Sexp.of_string)))
      ~doc:[%string "STRING set case match kind (see man fzf)"]
  and expect =
    flag
      "expect"
      (optional (Arg_type.comma_separated string))
      ~doc:"STRING comma-separated keys to use to select an entry (see man fzf)"
    |> Command.Param.map ~f:(fun expect_keys ->
      let%map.Option expect_keys in
      Fzf.Expect.{ expect_keys = Nonempty_list.of_list_exn expect_keys; key_pressed })
  in
  { query
  ; header
  ; select1
  ; no_sort
  ; reverse_input
  ; prompt_at_top
  ; with_nth
  ; height
  ; bind
  ; tiebreak
  ; case_match
  ; expect
  }
;;

let print_picked choice =
  printf !"Picked: %{sexp:string option}\n" choice;
  match Set_once.get key_pressed with
  | None -> ()
  | Some key_pressed -> printf !"Key pressed: %s\n" key_pressed
;;

let blocking =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Example fzf command (blocking implementation)"
    [%map_open
      let { query
          ; header
          ; select1
          ; no_sort
          ; reverse_input
          ; prompt_at_top
          ; with_nth
          ; height
          ; bind
          ; tiebreak
          ; case_match
          ; expect
          }
        =
        param
      and options = options_param in
      fun () ->
        Fzf.Blocking.pick_one
          ?query
          ?header
          ?select1
          ?no_sort
          ?reverse_input
          ?prompt_at_top
          ?with_nth
          ?height
          ?bind
          ?tiebreak
          ?case_match
          ?expect
          (Inputs options)
        |> print_picked]
;;

open! Async

let async =
  let open Command.Let_syntax in
  Command.async_or_error
    ~summary:"Example fzf command (async implementation)"
    [%map_open
      let { query
          ; header
          ; select1
          ; no_sort
          ; reverse_input
          ; prompt_at_top
          ; with_nth
          ; height
          ; bind
          ; tiebreak
          ; case_match
          ; expect
          }
        =
        param
      and options = options_param in
      fun () ->
        let open Deferred.Or_error.Let_syntax in
        Fzf.pick_one
          ?query
          ?header
          ?select1
          ?no_sort
          ?reverse_input
          ?prompt_at_top
          ?with_nth
          ?height
          ?bind
          ?tiebreak
          ?case_match
          ?expect
          (Inputs options)
        >>| print_picked]
    ~behave_nicely_in_pipeline:false
;;

let variable_input_command =
  let open Command.Let_syntax in
  Command.async_or_error
    ~summary:"Select from many strings"
    [%map_open
      let num_options = anon ("NUM-OPTIONS" %: int)
      and option_length =
        flag_optional_with_default_doc_sexp
          "option-length"
          int
          ~default:1
          [%sexp_of: int]
          ~doc:"_ Length of each individual option"
      in
      fun () ->
        let open Deferred.Or_error.Let_syntax in
        let option = String.make option_length 'a' in
        let options = List.init num_options ~f:(fun _ -> option) in
        Fzf.pick_one (Assoc (List.map options ~f:(fun x -> x, x))) >>| print_picked]
    ~behave_nicely_in_pipeline:false
;;

let pick_many_command =
  let open Command.Let_syntax in
  Command.async_or_error
    ~summary:"Fzf with pick many"
    [%map_open
      let { query
          ; header
          ; select1
          ; no_sort
          ; reverse_input
          ; prompt_at_top
          ; with_nth
          ; height
          ; bind
          ; tiebreak
          ; case_match
          ; expect
          }
        =
        param
      and options = options_param in
      fun () ->
        let open Deferred.Or_error.Let_syntax in
        Fzf.pick_many
          ?query
          ?header
          ?select1
          ?no_sort
          ?reverse_input
          ?prompt_at_top
          ?with_nth
          ?height
          ?bind
          ?tiebreak
          ?case_match
          ?expect
          (Inputs options)
        >>| Option.map ~f:[%sexp_of: string list]
        >>| Option.map ~f:Sexp.to_string
        >>| print_picked]
    ~behave_nicely_in_pipeline:false
;;

let from_map =
  let open Command.Let_syntax in
  Command.async_or_error
    ~summary:"Fzf pick from a map"
    [%map_open
      let { query
          ; header
          ; select1
          ; no_sort
          ; reverse_input
          ; prompt_at_top
          ; with_nth
          ; height
          ; bind
          ; tiebreak
          ; case_match
          ; expect
          }
        =
        param
      and options = options_param in
      fun () ->
        let open Deferred.Or_error.Let_syntax in
        let module Data = struct
          type t =
            | One
            | Two
          [@@deriving enumerate, sexp_of]
        end
        in
        let map =
          let data_seq = Sequence.cycle_list_exn Data.all in
          let option_seq = Sequence.of_list options in
          Sequence.zip option_seq data_seq |> Sequence.to_list |> String.Map.of_alist_exn
        in
        Fzf.pick_one
          ?query
          ?header
          ?select1
          ?no_sort
          ?reverse_input
          ?prompt_at_top
          ?with_nth
          ?height
          ?bind
          ?tiebreak
          ?case_match
          ?expect
          (Map map)
        >>| Option.map ~f:Data.sexp_of_t
        >>| Option.map ~f:Sexp.to_string
        >>| print_picked]
    ~behave_nicely_in_pipeline:false
;;

let from_command_output =
  let default_command = [%string "echo {q} | /bin/tr [:lower:] [:upper:]"] in
  Command.async_or_error
    ~summary:"Fzf pick from a command's output"
    ~readme:(fun () ->
      [%string
        "Mentions of '{q}' in COMMAND are replaced with the value of fzf's filter \
         string.  If COMMAND is ommited the following command is used by default:\n\n\
        \  %{default_command} "])
    (let%map_open.Command { query
                          ; header
                          ; select1
                          ; no_sort
                          ; reverse_input
                          ; prompt_at_top
                          ; with_nth
                          ; height
                          ; bind
                          ; tiebreak
                          ; case_match
                          ; expect
                          }
       =
       param
     and command = anon (maybe_with_default default_command ("command" %: string)) in
     fun () ->
       let open Deferred.Or_error.Let_syntax in
       Fzf.pick_one
         ?query
         ?header
         ?select1
         ?no_sort
         ?reverse_input
         ?prompt_at_top
         ?with_nth
         ?height
         ?bind
         ?tiebreak
         ?case_match
         ?expect
         (Command_output command)
       >>| print_picked)
    ~behave_nicely_in_pipeline:false
;;

let streaming =
  Command.async_or_error
    ~summary:"Stream integers infinitely into fzf"
    (let%map_open.Command { query
                          ; header
                          ; select1
                          ; no_sort
                          ; reverse_input
                          ; prompt_at_top
                          ; with_nth
                          ; height
                          ; bind
                          ; tiebreak
                          ; case_match
                          ; expect
                          }
       =
       param
     in
     fun () ->
       let open Deferred.Or_error.Let_syntax in
       let reader =
         Pipe.create_reader ~close_on_exception:true (fun writer ->
           Deferred.repeat_until_finished 0 (fun i ->
             if Pipe.is_closed writer
             then Deferred.return (`Finished ())
             else (
               Pipe.write_without_pushback writer (Int.to_string i);
               Deferred.return (`Repeat (succ i)))))
       in
       Fzf.pick_one
         ?query
         ?header
         ?select1
         ?no_sort
         ?reverse_input
         ?prompt_at_top
         ?with_nth
         ?height
         ?bind
         ?tiebreak
         ?case_match
         ?expect
         (Streaming (Fzf.Streaming.of_strings_raise_on_newlines reader))
       >>| print_picked)
    ~behave_nicely_in_pipeline:false
;;

let () =
  Command_unix.run
  @@ Command.group
       ~summary:"Fzf test utility"
       [ "async", async
       ; "blocking", blocking
       ; "variable-input", variable_input_command
       ; "pick-many", pick_many_command
       ; "from-map", from_map
       ; "from-command-output", from_command_output
       ; "streaming", streaming
       ]
;;
