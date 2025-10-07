open! Core
open! Async
open! Import

module Action : sig
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

module Tmux : sig
  val write_command_autocompletion
    :  prompt:string
    -> exe:string
    -> rcfile:string
    -> unit Deferred.t

  val dump_until
    :  ?debug:bool
    -> ?suppress_stdout:unit
    -> complete_exe:string
    -> until:[ `Exact of string | `Substring of string ]
    -> Tmux.t
    -> unit Deferred.t

  val from_fresh_prompt_tab_complete
    :  ?debug:bool
    -> prompt:string
    -> until:[< `Exact of string | `Substring of string ]
    -> complete_exe:string
    -> after_exe:string
    -> Tmux.t
    -> unit Deferred.t

  val clear_prompt : Tmux.t -> unit Deferred.t
  val tab_complete : Tmux.t -> unit Deferred.t
end

val test_bash
  :  ?width:int
  -> ?height:int
  -> bash_cmd:(root:string -> string)
  -> Action.t list
  -> unit Deferred.t

val test
  :  string (** name command *)
  -> ?select1:unit
  -> ?header:string
  -> ?query:string
  -> ?no_sort:unit
  -> ?reverse_input:unit
  -> ?prompt_at_top:unit
  -> ?with_nth:string
  -> ?height:int
  -> ?bind:string
  -> ?tiebreak:string
  -> ?escaped:unit
  -> ?case_match:string
  -> ?expect:string
  -> string list
  -> Action.t list
  -> unit Deferred.t

val test_no_options : string (** name command *) -> Action.t list -> unit Deferred.t
val options : string list
val test_blocking_and_async : (string -> unit Deferred.t) -> unit Deferred.t
val test_pick_many : (string -> unit Deferred.t) -> unit Deferred.t
val test_pick_map : (string -> unit Deferred.t) -> unit Deferred.t
