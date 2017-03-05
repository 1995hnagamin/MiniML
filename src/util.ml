let rec fold_right f y = function
    [] -> y
  | x::xs -> f x (fold_right f y xs)

let rec fold_left f y = function
    [] -> y
  | x::xs -> fold_left f (f y x) xs

let rec map f xs = match xs with
    [] -> []
  | x::xs -> (f x)::(map f xs)

let rec assoc_set key value = function
    [] -> [(key, value)]
  | (k,v)::rest ->
      if k = key
      then (k,value)::rest
      else (k,v)::(assoc_set key value rest)

let rec try_assoc key default alist =
  try List.assoc key alist with
  Not_found -> default
;;

let rec filter p = function
    [] -> []
  | x::xs -> if p x then x::(filter p xs) else filter p xs
;;
