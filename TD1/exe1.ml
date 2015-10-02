[1;2;3;4;5;6;7];; 
type 'a  tree =
	| Leaf of 'a
	| Node of 'a tree * 'a tree;; (* left_tree * right_tree *)

type 'a blist = (int * 'a tree) list;;

(* constree : int 'a tree -> 'a blist -> 'a blist *)

let rec constree h a p = match p with
| [] -> [a]
| (h', a') :: p' ->
	if h = h' then
		contree (h+1) (Node(0,0)) p'
	else
		(h,a)::p;;

Leaf "1";;


(* 1 lsl n = 2n *)