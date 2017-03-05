open Syntax
open Eval

let rec print_decl = function
    [] -> ();
  | (id,v)::rest ->
      Printf.printf "val %s : " id;
      pp_val v;
      print_newline();
      print_decl rest

let eval env program =
  let decl = Parser.toplevel Lexer.main program in
  eval_decl env decl

let eval_print env program =
  let (decls, newenv) =
    try eval env program with
      Eval.Error msg ->
        Printf.printf "Error: %s\n" msg;
        ([], env)
    | Parsing.Parse_error ->
        print_string "Error: Parse Error\n";
        ([], env)
    | Failure msg ->
        Printf.printf "Error: %s\n" msg;
        ([], env)
  in
  print_decl decls;
  newenv
;;

let read_eval_print env =
  print_string "# ";
  flush stdout;
  eval_print env (Lexing.from_channel stdin)

let rec read_eval_print_loop env =
  let newenv = read_eval_print env in
  read_eval_print_loop newenv

let exec_file filename =
  let ic = open_in filename in
  eval_print Environment.empty (Lexing.from_channel ic)

let rec make_env = function
    [] -> Environment.empty
  | (id, v)::rest -> Environment.extend id v (make_env rest)

let roman = [
    ("i",   IntV 1);
    ("ii",  IntV 2);
    ("iii", IntV 3);
    ("iv",  IntV 4);
    ("v",   IntV 5);
    ("x",   IntV 10);
    ]

let initial_env = make_env roman

let _ =
  if Array.length Sys.argv >= 2
  then exec_file Sys.argv.(1)
  else read_eval_print_loop initial_env
