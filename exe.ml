let list = [1;2;3];;

let rec sumList list = match list with
  | [] -> 0
  | a::b -> a + (sumList b);;

print_int (sumList list);;

type  variable_index  =  int
type  bdt  =
|  True
|  False
|  If of variable_index * bdt * bdt
 
(*
Table of : n XOR m
   n   0   1
	-----------
 m= 0  0   1
 m= 1  1   0

Impl = implies
Iff = if and only if
*)

type binop = And | Or | Xor | Impl | Iff
type formula =
| Cst of bool
| Var of variable_index
| Neg of formula
| Binop of binop * formula * formula

let rec bdt_of_formula : formula -> bdt = function
	| Cst(b) -> if b then True else False
	| Var(y) -> variable_index
	| Neg(f) -> reverse(f)
	| Binop(bn,f1,f2) -> convert(bn,f1,f2)

let rec reverse : formula -> bdt = function
	| Cst(b) -> if b then False else True
	| Var(y) -> variable_index
	| Neg(f) -> reverse(f)
	| Binop(bn,f1,f2) -> (bn,reverse(f1),reverse(f2))

let rec convert bn f1 f2 = match bn,f1,f2 with
| if bn = And then 

