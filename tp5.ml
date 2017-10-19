let rec rev = function
| [] -> []
| a::r -> (rev r)@[a];;

let rec rev_append l acc =
match l with
| [] -> acc
| a::l' -> rev_append l' (a::acc);;

let rev' l = rev_append l [];;

(**
Recurrence sur les entiers :
Enonce(o)
Il exists n Enonce(n) => Enonce(n+1)

Ici sur les listes :
-> Enonce([])
-> Il exists P et x, Enonce(P) => Enonce(x::P)

**)

module type DEQUE = sig
type 'a queue

val empty : 'a queue
val is_empty : 'a queue -> bool

val cons : 'a -> 'a queue -> 'a queue
val decons : 'a queue -> 'a * 'a queue

val snoc : 'a queue -> 'a -> 'a queue
val snoced : 'a queue -> 'a queue * 'a
end;;

module Stack : DEQUE = struct
type 'a queue = 'a list

let empty = []

let is_empty q = match q with
| [] -> true
| _ -> false

let cons ele q = ele::q

let decons q = match q with
| [] -> raise Not_found
| h::rest -> (h, rest)

let snoc q ele = rev' (ele::q)

let snoced q = 
let revq = (rev' q) in 
let (x,l) = decons revq in
(rev' l, x)

end;;


let _ = print_string "g\n"; fun x -> (x; print_string "d\n"; 42;)
