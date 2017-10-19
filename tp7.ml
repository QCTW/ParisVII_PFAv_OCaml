(**
let f x = x+1
=> bind name f with an anymous function x-> x+1 built automatically

let f = func x -> x+1
=> bind name f to an anymious function x-> x+1

fun x -> x+1
=> an anyomious function of x->x+1


let f = function 
  x -> x + 1
=> a function named f with one parameter x which execute x-> x+1
=> Most of time, followed with pattern-matching starting with pipe  (|)
=> Ex let f = function
            | X -> Y...
**)
type variable_index = int

type dbt =
| True
| False
| If of variable_index * bdt * bdt

type binop = And | Or | Xor | Impl | Iff

type formula =
| Cst of bool
| Var of variable_index
| Neg of formula
| Binop of binop * formula * formula

let rec negation bdt =
match bdt with
| True -> False
| False -> True
| If(t, a1, a2) -> If(t, (negation a1), (negation a2))

(**
a -> b := not a or b
not(a->b) := not :(not a or b) := a and not b

a <-> b := (a->b) and (b->a)
not(a<->b) := not(a->b) or not(b->a) := (a and not b) or (b and not a)
**)

let rec bdt_of_formula formu = fun
match formu with
| Cst b -> if b then True else False
   (** 
   * Same as "if var then true else false"
   * Here "var" will always be the same value as boolean
   **)
| Var v -> If(v, True, False)
| Neg f -> negation( bdt_of_formula f )
(**| Binop(b, f1, f2) -> ??? **)


