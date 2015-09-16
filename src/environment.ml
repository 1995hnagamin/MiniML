type 'a t = (Syntax.id * 'a) list ;;

let empty = []
let extend x v env = (x,v)::env

exception Not_bound

let rec lookup x env =
  try List.assoc x env with 
    Not_found -> raise Not_bound 
;; 

let rec exists id = function
    [] -> false
  | (x,v)::rest -> (x = id) || exists id rest
;;

