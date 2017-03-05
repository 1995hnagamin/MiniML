open Syntax
open Eval
open Util

let rec print_decl tyenv = function
    [] -> ();
  | (id,v)::rest ->
      Printf.printf "val %s : " id;
      pp_ty (Environment.lookup id tyenv);
      print_string " = ";
      pp_val v;
      print_newline();
      print_decl tyenv rest

let eval env tyenv program =
  let decl = Parser.toplevel Lexer.main program in
  let tydecls = Typing.ty_decls tyenv decl in 
  let (decls, newenv) = eval_decl env decl in
  (tydecls, decls, newenv)
;;

let eval_print env tyenv program =
  let (tydecls, decls, env') =
    try eval env tyenv program with
      Eval.Error msg ->
        Printf.printf "Error: %s\n" msg;
        (tyenv, [], env)
    | Typing.Error msg ->
        Printf.printf "Type Error : %s\n" msg;
        (tyenv, [], env)
    | Parsing.Parse_error ->
        print_string "Error: Parse Error\n";
        (tyenv, [], env)
    | Failure msg ->
        Printf.printf "Error: %s\n" msg;
        (tyenv, [], env)
  in
  let tyenv' = fold_left 
    (fun env (x,t) -> Environment.extend x t env) tyenv tydecls in
  print_decl tydecls decls;
  (env', tyenv')
;;

let read_eval_print env tyenv =
  print_string "# ";
  flush stdout;
  eval_print env tyenv (Lexing.from_channel stdin)

let rec read_eval_print_loop env tyenv =
  let (env', tyenv') = read_eval_print env tyenv in
  read_eval_print_loop env' tyenv'

let exec_file filename tyenv =
  let ic = open_in filename in
  eval_print Environment.empty tyenv (Lexing.from_channel ic)

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

let tyroman = [
    ("i",  TyInt);
    ("ii", TyInt);
    ("iii",TyInt);
    ("iv", TyInt);
    ("v",  TyInt);
    ("x",  TyInt);
    ]
;;

let initial_tyenv = make_env tyroman

let _ =
  if Array.length Sys.argv >= 2
  then exec_file Sys.argv.(1)
  else read_eval_print_loop initial_env initial_tyenv
