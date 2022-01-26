open! Core
open! Async
open! Import

type query = unit [@@deriving bin_io, sexp]
type response = string [@@deriving bin_io, sexp]
type error = unit [@@deriving bin_io, sexp]

let pipe_rpc =
  Rpc.Pipe_rpc.create
    ~name:"send a string for fzf"
    ~version:0
    ~bin_query:[%bin_type_class: unit]
    ~bin_response:[%bin_type_class: string]
    ~bin_error:[%bin_type_class: unit]
    ()
;;
