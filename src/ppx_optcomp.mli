(** Optional compilation with cpp-like directives *)

open Ppx_core

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
  type t

  val init : t

  val add : t -> var:string Loc.t -> value:Value.t -> t

  val of_list : (string Loc.t * Value.t) list -> t
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
  val preprocess_file : string -> Out_channel.t -> unit
end
