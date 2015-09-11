type 'a t = (Syntax.id * 'a) list ;;

let empty = []
let extend x v env = (x,v)::env

exception Not_bound

let rec lookup x env =
  try List.assoc x env with Not_bound -> raise Not_bound 
;;
    
let rec map f = function
    [] -> []
  | (id, v)::rest -> (id, f v) :: map f rest
;;

let rec fold_right f env a =
  match env with
    [] -> a
  | (_, v)::rest -> f v (fold_right f rest a)
;;
