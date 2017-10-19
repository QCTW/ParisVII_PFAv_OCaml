(* 
somme (somme (somme 2 3) 4) (somme 2 (somme 3 4));;
*)

let checktauto f = 
  if f true then true else false;; (* Pas correct *)

let checktauto f =
  (f true) && (f false);; (* Correct *)

(* Not correct
let checktauto2 f =
  (checktauto (f true) && checktauto (f false));; *)

let checktauto2 f =
  checktauto (fun b -> checktauto (f b));;

(* Not correct
let checktauto3 f =
  (checktauto2 (f true) && checktauto2 (f false));; *)

let checktauto3 f =
  checktauto (fun b -> checktauto2 (f b));;

(*
The negative is not ! in OCaml, it's NOT!
*)
let g x y z =
  if y then
    if not x then 2 else 
    if z then 4 else 3
  else 
  if z then 1 else 3;;

let f x y z = match x, y, z with
  | _, false, true -> 1
  | false, true, _ -> 2
  | _, _, false -> 3
  | _, _, true -> 4;;

let _ = 
  checktauto3 (fun x y z -> f x y z = g x y z)

let rec append l1 l2 =
match l2 with
  | [] -> l1
  | e::rest -> e::append rest l2

let rec flatten li =
match li with
  | [] -> []
  | h::rest -> h@(faltten rest)


let rec rev l =
match l with
  | [] -> []
  | h::rest -> (rev rest)::h


let rec somme_liste = function
  | [] -> 0
  | t::q -> t + somme_liste q;;

let rec fold_left f acc = function
  | [] -> acc
  | t::q -> fold_left f (f acc t) q;;

let somme_liste li = 
  fold_left (+) 0 li;;

let scal v v' = List.fold_left (+) 0 (List.map2 ( * ) v v');;)
