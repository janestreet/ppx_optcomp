(** Optional compilation with cpp-like directives *)

open StdLabels
open Ppx_core.Std
open Parsetree

module Value : sig
  (** Representation of values supported by optcomp. *)
  type t =
    | Bool   of bool
    | Int    of int
    | Char   of char
    | String of string
    | Tuple  of t list

  val to_string : t -> string
  val to_string_pretty : t -> string

  val to_expression : Location.t -> t -> expression
  val to_pattern    : Location.t -> t -> pattern

  val ocaml_version : t
end

module Env : sig
  type t = Value.t Map.Make(String).t

  include Map.S with type key = string and type 'a t := 'a Map.Make(String).t

  val init : t
end

val eval : Env.t -> expression -> Value.t
(** [eval env expr] tries to evalute [expr] in [env]. It raises if the expression cannot
    be statically evaluated. *)

module Make(Params : sig
    val lexer : Lexing.lexbuf -> Parser.token
    val env : Env.t
  end) : sig
  val init : unit -> unit
  val map_lexer : (Lexing.lexbuf -> Parser.token) -> Lexing.lexbuf -> Parser.token
  val preprocess_file : string -> out_channel -> unit
end
