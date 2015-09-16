open Util

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

let resolve env pairs =
  let rec f alist = function
    [] -> alist
    | (x,_)::rest ->
        let v = lookup x env in
        f (assoc_set x v alist) rest
  in
  f [] pairs
;;

