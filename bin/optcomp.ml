open Ppx_core

module M = Ppx_optcomp.Make(struct
    let lexer = Lexer.token
    let env = Ppx_optcomp.Env.init
  end)

let () =
  let usage =
    Printf.sprintf "%s [extra_args] [<files>]" Caml.Sys.executable_name
  in
  Lexer.set_preprocessor ignore (fun lexer -> lexer);
  try
    Caml.Arg.parse [] (fun fn -> M.preprocess_file fn stdout) usage
  with exn ->
    Location.report_exception Caml.Format.err_formatter exn;
    Caml.exit 1
;;
