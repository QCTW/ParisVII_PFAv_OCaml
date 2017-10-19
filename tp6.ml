exception Empty

module type STREAM = sig
  type 'a t = ('a cell) Lazy.t
  and 'a cell = Cons of 'a * 'a t

  val empty : 'a t
  val cons : 'a -> 'a t -> 'a t
  val get : 'a t -> 'a * 'a t
end

module MyStream : STREAM = struct
  type 'a t = ('a cell) Lazy.t
  and 'a cell = Cons of 'a * 'a t

  let empty = lazy (raise Empty)
  let cons x s = lazy (Cons(x, s) )
  let get s = match s with lazy (Cons (x,s')) -> (x, s')
end

let rec of_list l =
match l with
| [] -> MyStream.empty
| x::li -> MyStream.cons x (of_list li)

let of_list l = List.fold_right MyStream.cons l MyStream.empty

let rec take num t = 
if num = 0 then [] else
try
  let (x, s) = MyStream.get t in 
  x::(take (num-1) s)
with Empty -> []

let rec bad_from n = MyStream.cons n (bad_from (n+1))

let rec from n = 

