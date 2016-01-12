open StdLabels
open Ppx_core.Std
open Ast_builder.Default
open Parsetree

module Type = struct
  type t =
    | Var of string
    | Bool
    | Int
    | Char
    | String
    | Tuple of t list

  let rec to_string = function
    | Var v   -> "'" ^ v
    | Bool    -> "bool"
    | Int     -> "int"
    | Char    -> "char"
    | String  -> "string"
    | Tuple l -> "(" ^ String.concat ~sep:" * " (List.map l ~f:to_string) ^ ")"
end

module Value = struct

  type t =
    | Bool   of bool
    | Int    of int
    | Char   of char
    | String of string
    | Tuple  of t list

  let ocaml_version =
    Scanf.sscanf Sys.ocaml_version "%d.%d.%d"
      (fun major minor patchlevel -> Tuple [Int major; Int minor; Int patchlevel])
  ;;

  let rec to_expression loc t =
    match t with
    | Bool   x   -> ebool   ~loc x
    | Int    x   -> eint    ~loc x
    | Char   x   -> echar   ~loc x
    | String x   -> estring ~loc x
    | Tuple  []  -> eunit   ~loc
    | Tuple  [x] -> to_expression loc x
    | Tuple  l   -> pexp_tuple ~loc (List.map l ~f:(to_expression loc))
  ;;

  let rec to_pattern loc t =
    match t with
    | Bool   x   -> pbool   ~loc x
    | Int    x   -> pint    ~loc x
    | Char   x   -> pchar   ~loc x
    | String x   -> pstring ~loc x
    | Tuple  []  -> punit   ~loc
    | Tuple  [x] -> to_pattern loc x
    | Tuple  l   -> ppat_tuple ~loc (List.map l ~f:(to_pattern loc))
  ;;

  let to_string_pretty v =
    let e = to_expression Location.none v in
    Pprintast.string_of_expression e

  let to_string v =
    let buf = Buffer.create 128 in
    let rec aux = function
      | Bool b ->
        Buffer.add_string buf (string_of_bool b)
      | Int n ->
        Buffer.add_string buf (string_of_int n)
      | Char ch ->
        Buffer.add_char buf ch
      | String s ->
        Buffer.add_string buf s;
      | Tuple [] ->
        Buffer.add_string buf "()"
      | Tuple (x :: l) ->
        Buffer.add_char buf '(';
        aux x;
        List.iter l ~f:(fun x ->
          Buffer.add_string buf ", ";
          aux x);
        Buffer.add_char buf ')'
    in
    aux v;
    Buffer.contents buf
  ;;

  let rec type_ : t -> Type.t = function
    | Bool   _ -> Bool
    | Int    _ -> Int
    | Char   _ -> Char
    | String _ -> String
    | Tuple  l -> Tuple (List.map l ~f:type_)
  ;;
end

module Env = struct
  include (Map.Make(String)
           : Map.S with type key = string and type 'a t := 'a Map.Make(String).t)

  type t = Value.t Map.Make(String).t

  let init = singleton "ocaml_version" Value.ocaml_version
end

module Directive = struct
  type t =
    | Let     of pattern * expression
    | If      of expression
    | Else
    | Elif    of expression
    | Endif
    | Import  of string
    | Error   of expression
    | Warning of expression
end

(* +-----------------------------------------------------------------+
   | Import resolution                                               |
   +-----------------------------------------------------------------+ *)

let resolve_import ~current_filename ~filename =
  let dir = Filename.dirname current_filename in
  if Filename.is_relative filename && dir <> "." then
    Filename.concat dir filename
  else
    filename

(* +-----------------------------------------------------------------+
   | Expression evaluation                                           |
   +-----------------------------------------------------------------+ *)

let invalid_type loc expected real =
  Location.raise_errorf ~loc
    "optcomp: this expression has type %s but is used with type %s"
    (Type.to_string real) (Type.to_string expected)
;;

let var_of_lid (id : _ Located.t) =
  match Longident.flatten id.txt with
  | l -> { id with txt = String.concat ~sep:"." l }
  | exception _ ->
    Location.raise_errorf ~loc:id.loc "optcomp: invalid variable name"
;;

let cannot_convert loc dst x =
  Location.raise_errorf ~loc "cannot convert %s to %s" (Value.to_string_pretty x) dst
;;

let convert_from_string loc dst f x =
  try
    f x
  with _ ->
    Location.raise_errorf ~loc "optcomp: cannot convert %S to %s" x dst
;;

exception Pattern_match_failure of pattern * Value.t

let lid_of_expr e =
  match e.pexp_desc with
  | Pexp_ident id | Pexp_construct (id, None) -> id
  | _ -> Location.raise_errorf ~loc:e.pexp_loc "optcomp: identifier expected"
;;

let var_of_expr e = var_of_lid (lid_of_expr e)

let not_supported e =
  Location.raise_errorf ~loc:e.pexp_loc "optcomp: expression not supported"
;;

let rec eval env e : Value.t =
  let loc = e.pexp_loc in
  match e.pexp_desc with
  | Pexp_constant (Const_int     x    ) -> Int    x
  | Pexp_constant (Const_char    x    ) -> Char   x
  | Pexp_constant (Const_string (x, _)) -> String x

  | Pexp_construct ({ txt = Lident "true" ; _ }, None) -> Bool true
  | Pexp_construct ({ txt = Lident "false"; _ }, None) -> Bool false
  | Pexp_construct ({ txt = Lident "()"   ; _ }, None) -> Tuple []

  | Pexp_tuple l -> Tuple (List.map l ~f:(eval env))

  | Pexp_ident id | Pexp_construct (id, None) -> begin
      let name = var_of_lid id in
      match Env.find name.txt env with
      | v -> v
      | exception Not_found ->
        Location.raise_errorf ~loc "optcomp: unbound value %s" name.txt
    end

  | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident s; _ }; _ }, args) -> begin
      let args =
        List.map args ~f:(fun (l, x) -> if l <> "" then not_supported e else x)
      in
      match s, args with
      | "="  , [x; y] -> eval_cmp     env ( = )   x y
      | "<"  , [x; y] -> eval_cmp     env ( < )   x y
      | ">"  , [x; y] -> eval_cmp     env ( > )   x y
      | "<=" , [x; y] -> eval_cmp     env ( <= )  x y
      | ">=" , [x; y] -> eval_cmp     env ( >= )  x y
      | "<>" , [x; y] -> eval_cmp     env ( <> )  x y
      | "min", [x; y] -> eval_poly2   env min     x y
      | "max", [x; y] -> eval_poly2   env max     x y
      | "+"  , [x; y] -> eval_int2    env ( + )   x y
      | "-"  , [x; y] -> eval_int2    env ( - )   x y
      | "*"  , [x; y] -> eval_int2    env ( * )   x y
      | "/"  , [x; y] -> eval_int2    env ( / )   x y
      | "mod", [x; y] -> eval_int2    env ( mod ) x y
      | "not", [x]    -> Bool (not (eval_bool env x))
      | "||" , [x; y] -> eval_bool2   env ( || ) x y
      | "&&" , [x; y] -> eval_bool2   env ( && ) x y
      | "^"  , [x; y] -> eval_string2 env ( ^ )  x y
      | "fst", [x]    -> fst (eval_pair env x)
      | "snd", [x]    -> snd (eval_pair env x)
      | "to_string", [x] ->
        String (Value.to_string (eval env x))
      | "to_int", [x] ->
        Int
          (match eval env x with
           | String x -> convert_from_string loc "int" int_of_string x
           | Int    x -> x
           | Char   x -> int_of_char x
           | Bool _ | Tuple _ as x -> cannot_convert loc "int" x)
      | "to_bool", [x] ->
        Bool
          (match eval env x with
           | String x -> convert_from_string loc "bool" bool_of_string x
           | Bool   x -> x
           | Int _ | Char _ | Tuple _ as x -> cannot_convert loc "bool" x)
      | "to_char", [x] ->
        Char
          (match eval env x with
           | String x ->
             convert_from_string loc "char"
               (fun s -> assert (String.length s = 1); s.[0]) x
           | Char x -> x
           | Int x ->
             begin try
               Char.chr x
             with _ ->
               Location.raise_errorf ~loc "optcomp: cannot convert %d to char" x
             end
           | Bool _ | Tuple _ as x -> cannot_convert loc "char" x)
      | "show", [x] -> String (Value.to_string_pretty (eval env x))
      | "defined", [x] -> Bool (Env.mem (var_of_expr x).txt env)
      | _ -> not_supported e
    end

  (* Let-binding *)
  | Pexp_let (Nonrecursive, vbs, e) ->
    let env =
      List.fold_left vbs ~init:env ~f:(fun new_env vb ->
        let v = eval env vb.pvb_expr in
        do_bind new_env vb.pvb_pat v)
    in
    eval env e

  (* Pattern matching *)
  | Pexp_match (e, cases) ->
    let v = eval env e in
    let rec loop = function
      | [] ->
        Location.raise_errorf ~loc "optcomp: cannot match %s against any of the cases"
          (Value.to_string v)
      | case :: rest ->
        match bind env case.pc_lhs v with
        | exception Pattern_match_failure _ -> loop rest
        | env ->
          let guard_ok =
            match case.pc_guard with
            | None   -> true
            | Some e -> eval_bool env e
          in
          if guard_ok then
            eval env case.pc_rhs
          else
            loop rest
    in
    loop cases

  | _ -> not_supported e

and bind env patt value =
  match patt.ppat_desc, value with
  | Ppat_any, _ -> env

  | Ppat_constant (Const_int     x    ), Int    y when x = y -> env
  | Ppat_constant (Const_char    x    ), Char   y when x = y -> env
  | Ppat_constant (Const_string (x, _)), String y when x = y -> env

  | Ppat_construct ({ txt = Lident "true" ; _ }, None), Bool true  -> env
  | Ppat_construct ({ txt = Lident "false"; _ }, None), Bool false -> env
  | Ppat_construct ({ txt = Lident "()"   ; _ }, None), Tuple []   -> env

  | Ppat_var { txt; _ }, _       -> Env.add txt value env
  | Ppat_construct (id, None), _ -> Env.add (var_of_lid id).txt value env

  | Ppat_alias (patt, { txt; _ }), _ ->
    Env.add txt value (bind env patt value)

  | Ppat_tuple x, Tuple y when List.length x = List.length y ->
    List.fold_left2 x y ~init:env ~f:bind

  | _ ->
    raise (Pattern_match_failure (patt, value))

and do_bind env patt value =
  try
    bind env patt value
  with Pattern_match_failure (pat, v) ->
    Location.raise_errorf ~loc:pat.ppat_loc
      "Cannot match %s with this pattern" (Value.to_string_pretty v)

and eval_same env ex ey =
  let vx = eval env ex and vy = eval env ey in
  let tx = Value.type_ vx and ty = Value.type_ vy in
  if tx = ty then
    (vx, vy)
  else
    invalid_type ey.pexp_loc tx ty

and eval_int env e =
  match eval env e with
  | Int x -> x
  | v -> invalid_type e.pexp_loc Int (Value.type_ v)

and eval_bool env e =
  match eval env e with
  | Bool x -> x
  | v -> invalid_type e.pexp_loc Bool (Value.type_ v)

and eval_string env e =
  match eval env e with
  | String x -> x
  | v -> invalid_type e.pexp_loc String (Value.type_ v)

and eval_pair env e =
  match eval env e with
  | Tuple [x; y] -> (x, y)
  | v -> invalid_type e.pexp_loc (Tuple [Var "a"; Var "b"]) (Value.type_ v)

and eval_int2 env f a b =
  let a = eval_int env a in
  let b = eval_int env b in
  Int (f a b)

and eval_bool2 env f a b =
  let a = eval_bool env a in
  let b = eval_bool env b in
  Bool (f a b)

and eval_string2 env f a b =
  let a = eval_string env a in
  let b = eval_string env b in
  String (f a b)

and eval_cmp env f a b =
  let a, b = eval_same env a b in
  Bool (f a b)

and eval_poly2 env f a b =
  let a, b = eval_same env a b in
  f a b

type lexer = Lexing.lexbuf -> Parser.token

module Make(Params : sig val env : Env.t end) : sig
  val init : unit -> unit
  val map_lexer : lexer -> lexer
  val preprocess_file : string -> out_channel -> unit
end = struct
  (* +---------------------------------------------------------------+
     | Parsing of directives                                         |
     +---------------------------------------------------------------+ *)

  let located x lexbuf =
    { Location.
      txt = x
    ; loc = Location.curr lexbuf
    }
  ;;

  let parse parsing_fun lexer lexbuf =
    try
      parsing_fun lexer lexbuf
    with Parsing.Parse_error | Syntaxerr.Escape_error ->
      let loc = Location.curr lexbuf in
      raise (Syntaxerr.Error(Syntaxerr.Other loc))
  ;;

  let fetch_directive_argument (lexer : lexer) lexbuf
        (tokens : Parser.token Located.t list) =
    let rec loop acc (brackets : Parser.token list) =
      match lexer lexbuf, brackets with
      | EOF, _ | EOL, [] -> located Parser.EOF lexbuf :: acc
      | (EOL | COMMENT _), _ -> loop acc brackets
      | token, _ ->
        let acc = located token lexbuf :: acc in
        match token, brackets with
        | BEGIN        , _ -> loop acc (END             :: brackets)
        | DO           , _ -> loop acc (DONE            :: brackets)
        | LPAREN       , _ -> loop acc (RPAREN          :: brackets)
        | LBRACE       , _ -> loop acc (RBRACE          :: brackets)
        | LBRACELESS   , _ -> loop acc (GREATERRBRACE   :: brackets)
        | LBRACKETLESS , _ -> loop acc (GREATERRBRACKET :: brackets)
        | LBRACKETBAR  , _ -> loop acc (BARRBRACKET     :: brackets)
        | (LBRACKET
          | LBRACKETGREATER
          | LBRACKETPERCENT
          | LBRACKETPERCENTPERCENT
          | LBRACKETAT
          | LBRACKETATAT
          | LBRACKETATATAT), _ -> loop acc (RBRACKET :: brackets)
        | _, closing :: brackets when token = closing -> loop acc brackets
        | _ -> loop acc brackets
    in
    let start_pos =
      match tokens with
      | []         -> Lexing.lexeme_end_p lexbuf
      | token :: _ -> token.loc.loc_start
    in
    match loop tokens [] |> List.rev with
    | []     -> None
    | tokens ->
      let tokens = ref tokens in
      let fake_lexer (lexbuf : Lexing.lexbuf) : Parser.token =
        match !tokens with
        | [] -> EOF
        | token :: rest ->
          tokens := rest;
          lexbuf.lex_start_p <- token.loc.loc_start;
          lexbuf.lex_curr_p  <- token.loc.loc_end;
          token.txt
      in
      let fake_lexbuf = Lexing.from_function (fun _ _ -> assert false) in
      fake_lexbuf.lex_curr_p <- start_pos;
      match parse Parser.implementation fake_lexer fake_lexbuf with
      | []   -> None
      | [st] ->
        assert_no_attributes_in#structure_item st;
        Some st
      | _ :: st :: _ ->
        Location.raise_errorf ~loc:st.pstr_loc "optcomp: too many structure items"
  ;;

  let enot e =
    let loc = e.pexp_loc in
    eapply ~loc (evar ~loc "not") [e]
  ;;

  let parse_directive (lexer : lexer) lexbuf : Directive.t Located.t =
    let token = located (lexer lexbuf) lexbuf in
    let arg =
      let tokens =
        match token.txt with
        | LET -> [token]
        | _   -> []
      in
      fetch_directive_argument lexer lexbuf tokens
    in
    let loc = { token.loc with loc_end = Lexing.lexeme_end_p lexbuf } in
    let invalid () = Location.raise_errorf ~loc "optcomp: invalid directive syntax" in
    let get_expr () =
      match arg with
      | Some { pstr_desc = Pstr_eval (e, _); _ } -> e
      | _ -> invalid ()
    in
    let get_def () =
      let e = get_expr () in
      let loc = e.pexp_loc in
      eapply ~loc (evar ~loc "defined") [e]
    in
    let nothing () =
      match arg with
      | None -> ()
      | Some st ->
        Location.raise_errorf ~loc:st.pstr_loc
          "optcomp: directive argument not expected here"
    in
    let dir : Directive.t =
      match token.txt with
      | LET -> begin
          match arg with
          | Some { pstr_desc = Pstr_value (Nonrecursive, [ vb  ]); _ } ->
            Let (vb.pvb_pat, vb.pvb_expr)
          | _ -> invalid ()
        end

      | IF               -> If (get_expr ())
      | ELSE             -> nothing (); Else
      | LIDENT "elif"    -> Elif (get_expr ())
      | LIDENT "endif"   -> nothing (); Endif
      | LIDENT "error"   -> Error (get_expr ())
      | LIDENT "warning" -> Warning (get_expr ())

      | LIDENT "ifdef"    -> If   (get_def ())
      | LIDENT "elifdef"  -> Elif (get_def ())
      | LIDENT "ifndef"   -> If   (get_def () |> enot)
      | LIDENT "elifndef" -> Elif (get_def () |> enot)

      | LIDENT "define" -> begin
          let e = get_expr () in
          match e.pexp_desc with
          | Pexp_construct (x, Some y) ->
            let id = var_of_lid x in
            Let (ppat_var ~loc:id.loc id, y)
          | Pexp_apply (x, [("", y)]) ->
            let id = var_of_expr x in
            Let (ppat_var ~loc:id.loc id, y)
          | _ ->
            let id = var_of_expr e in
            Let (ppat_var ~loc:id.loc id, eunit ~loc:id.loc)
        end

      | LIDENT "import" -> begin
          let e = get_expr () in
          match e.pexp_desc with
          | Pexp_constant (Const_string (s, _)) -> Import s
          | _ ->
            Location.raise_errorf ~loc:e.pexp_loc "optcomp: #import expect a string"
        end

      | _ ->
        Location.raise_errorf ~loc:token.loc "optcomp: unknown directive"
    in
    { txt = dir; loc }

  (* +---------------------------------------------------------------+
     | Block skipping                                                |
     +---------------------------------------------------------------+ *)

  let endif_missing lexbuf =
    Location.raise_errorf ~loc:(Location.curr lexbuf)
      "optcomp: #endif missing"
  ;;

  let rec skip_line (lexer : lexer) lexbuf =
    match lexer lexbuf with
    | EOF -> endif_missing lexbuf
    | EOL -> ()
    | _   -> skip_line lexer lexbuf

  let rec next_directive (lexer : lexer) lexbuf =
    match lexer lexbuf with
    | SHARP -> parse_directive lexer lexbuf
    | EOL -> next_directive lexer lexbuf
    | EOF -> endif_missing lexbuf
    | _ -> skip_line lexer lexbuf; next_directive lexer lexbuf

  let rec next_endif lexer lexbuf =
    let dir = next_directive lexer lexbuf in
    match dir.txt with
    | If _ -> skip_if lexer lexbuf; next_endif lexer lexbuf
    | Else
    | Elif _
    | Endif -> dir
    | _     -> next_endif lexer lexbuf

  and skip_if lexer lexbuf =
    match (next_directive lexer lexbuf).txt with
    | If _   -> skip_if lexer lexbuf; skip_if lexer lexbuf
    | Else   -> skip_else lexer lexbuf
    | Elif _ -> skip_if lexer lexbuf
    | Endif  -> ()
    | _      -> skip_if lexer lexbuf

  and skip_else lexer lexbuf =
    match (next_directive lexer lexbuf).txt with
    | If _ -> skip_if lexer lexbuf; skip_else lexer lexbuf
    | Else ->
      Location.raise_errorf ~loc:(Location.curr lexbuf) "optcomp: #else or without #if"
    | Elif _ ->
      Location.raise_errorf ~loc:(Location.curr lexbuf) "optcomp: #elif or without #if"
    | Endif -> ()
    | _ -> skip_else lexer lexbuf

  (* +---------------------------------------------------------------+
     | Token filtering                                               |
     +---------------------------------------------------------------+ *)

  module Stack = struct
    module Context = struct
      type t = If | Else
    end

    (* Opened blocks *)
    let stack : Context.t list ref = ref []

    let enqueue (dir : Directive.t Located.t) =
      let ctx : Context.t =
        match dir.txt with
        | If _ | Elif _ -> If
        | Else -> Else
        | _ -> assert false
      in
      stack := ctx :: !stack
    ;;

    let dequeue (dir : Directive.t Located.t) =
      match dir.txt, !stack with
      | (Else | Elif _) , If :: rest
      | Endif           , _  :: rest -> stack := rest
      | _ -> Location.raise_errorf ~loc:dir.loc "optcomp: no previous #if"
    ;;

    let save () = let state = !stack in stack := []; state
    let restore state = stack := state

    let is_empty () = !stack = []

    let check_eof lexbuf =
      if not (is_empty ()) then
        Location.raise_errorf ~loc:(Location.curr lexbuf) "optcomp: #endif missing"
    ;;

    let reset () = stack := []
  end

  let env = ref Params.env

  let at_bol lexbuf =
    let pos = Lexing.lexeme_start_p lexbuf in
    pos.pos_cnum = pos.pos_bol
  ;;

  exception Error_in_import of Lexing.position list * Location.error

  let map_exn = function
    | Error_in_import (stack, error) ->
      let locs = List.rev_map stack ~f:(fun (pos : Lexing.position) ->
        Printf.sprintf "imported from file \"%s\", line %i" pos.pos_fname pos.pos_lnum)
      in
      let msg = String.concat ~sep:"\n" (error.msg :: locs) in
      raise (Location.Error { error with msg })
    | exn -> raise exn

  (* Return the next token from a stream, interpreting directives. *)
  let rec lexer_internal (lexer : lexer) lexbuf : Parser.token =
    match lexer lexbuf with
    | SHARP when at_bol lexbuf ->
      interpret_directive lexer lexbuf (parse_directive lexer lexbuf);
      lexer_internal lexer lexbuf
    | EOF -> Stack.check_eof lexbuf; EOF
    | token -> token

  and lexer_internal_skip_eols lexer lexbuf =
    match lexer_internal lexer lexbuf with
    | EOL -> lexer_internal_skip_eols lexer lexbuf
    | token -> token

  and interpret_directive lexer lexbuf (dir : Directive.t Located.t) =
    match dir.txt with
    | If e ->
      if eval_bool !env e then
        Stack.enqueue dir
      else begin
        let dir = next_endif lexer lexbuf in
        match dir.txt with
        | Else   -> Stack.enqueue dir
        | Elif e -> interpret_directive lexer lexbuf { dir with txt = If e }
        | Endif  -> ()
        | _      -> assert false
      end

    | Else   -> Stack.dequeue dir; skip_else lexer lexbuf
    | Elif _ -> Stack.dequeue dir; skip_if   lexer lexbuf
    | Endif  -> Stack.dequeue dir

    | Let (patt, expr) ->
      env := do_bind !env patt (eval !env expr)

    | Import fname ->
      let fname =
        resolve_import
          ~current_filename:dir.loc.loc_start.pos_fname
          ~filename:fname
      in
      import ~loc:dir.loc lexer fname

    | Error e ->
      Location.raise_errorf ~loc:dir.loc "%s" (eval_string !env e)

    | Warning e ->
      let msg = eval_string !env e in
      let ppf = Format.err_formatter in
      Location.print ppf dir.loc;
      Format.fprintf ppf "Warning %s@." msg;
      Format.pp_print_flush ppf ()

  and import ~loc lexer fname =
    let ic =
      try
        open_in fname
      with exn ->
        let msg =
          match exn with
          | Sys_error msg -> msg
          | _ -> Printexc.to_string exn
        in
        Location.raise_errorf ~loc "optcomp: cannot open \"%s\": %s" fname msg
    in
    let lexbuf = Lexing.from_channel ic in
    Location.init lexbuf fname;
    let stack_state = Stack.save () in
    match lexer_internal_skip_eols lexer lexbuf with
    | exception e -> begin
        Stack.restore stack_state;
        close_in ic;
        match e with
        | Location.Error error ->
          raise (Error_in_import ([loc.loc_start], error))
        | Error_in_import (stack, error) ->
          raise (Error_in_import (loc.loc_start :: stack, error))
        | _ -> raise e
      end
    | token ->
      Stack.restore stack_state;
      close_in ic;
      match token with
      | EOF -> ()
      | _ ->
        let error =
          Location.errorf ~loc:(Location.curr lexbuf)
            "optcomp: only directives are allowed in imported files"
        in
        raise (Error_in_import ([loc.loc_start], error))

  let init = Stack.reset

  let map_lexer lexer lexbuf =
    try
      lexer_internal lexer lexbuf
    with exn ->
      map_exn exn

  let preprocess_file fn oc =
    let ic = open_in fn in
    let lexbuf = Lexing.from_channel ic in
    Location.init lexbuf fn;
    let rec loop pos acc =
      match Lexer.token lexbuf with
      | SHARP when at_bol lexbuf ->
        let acc = (pos, Lexing.lexeme_start lexbuf) :: acc in
        interpret_directive Lexer.token lexbuf (parse_directive Lexer.token lexbuf);
        loop (Lexing.lexeme_end lexbuf) acc
      | EOF -> Stack.check_eof lexbuf; (pos, Lexing.lexeme_end lexbuf) :: acc
      | _ -> loop pos acc
    in
    let chunks_to_copy =
      init ();
      try
        List.rev (loop 0 [])
      with exn ->
        map_exn exn
    in
    let buf = Buffer.create 4096 in
    List.iter chunks_to_copy ~f:(fun (start, stop) ->
      let len = stop - start in
      if len <> 0 then begin
        seek_in ic start;
        Buffer.add_channel buf ic len;
        Buffer.output_buffer oc buf;
        Buffer.clear buf;
      end);
    close_in ic;
  ;;
end
