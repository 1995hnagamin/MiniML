open Syntax
open Eval

let read_eval_print env =
  print_string "# ";
  flush stdout;
  let decl = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
  let (id, newenv, v) = eval_decl env decl in
  Printf.printf "val %s : " id;
  pp_val v;
  print_newline();
  newenv
;;

let rec read_eval_print_loop env =
  let newenv = 
    try read_eval_print env with
      Eval.Error msg -> Printf.printf "Error: %s\n" msg; env
    | Parsing.Parse_error -> print_string "Error: Parse Error\n"; env
    | Failure msg -> Printf.printf "Error: %s\n" msg; env
  in
  read_eval_print_loop newenv
;;

let rec make_env = function
    [] -> Environment.empty
  | (id, v)::rest -> Environment.extend id v (make_env rest)
;;

let roman = [
    ("i",   IntV 1);
    ("ii",  IntV 2);
    ("iii", IntV 3);
    ("iv",  IntV 4);
    ("v",   IntV 5);
    ("x",   IntV 10);
    ]
;;

let initial_env = make_env roman

let _ = read_eval_print_loop initial_env

