open Util

type 'a t = 'a list ;;

let void = []

let singleton x = [x]

let rec elem y = function
    [] -> false
  | x::xs -> x = y || elem y xs
;;

let unite xs ys = 
  fold_left (fun ys x -> if elem x ys then ys else x::ys) ys xs
;;

let intersect xs ys = filter (fun x -> elem x ys) xs
