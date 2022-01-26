open! Core

module Egg_choice = struct
  module T = struct
    type t =
      | Poached
      | Scrambled
      | Sunny_side_up
    [@@deriving enumerate, sexp]
  end

  include T
  include Enum.Make_stringable (T)
end

let arg_type =
  Command.Arg_type.enumerated
    ~auto_complete:(Fzf.complete_enumerable (module Egg_choice))
    (module Egg_choice)
;;

let command =
  Command.basic
    ~summary:"Example command with fzf-completed arguments"
    (let%map_open.Command egg_choice =
       flag "-egg-choice" (required arg_type) ~doc:"Choose how you'd like your eggs done"
     in
     fun () ->
       print_endline [%string "Good choice, having your eggs %{egg_choice#Egg_choice}!"])
;;

let () = Command_unix.run command
