open Tp1;;

module type RALIST = sig
  type 'a t
  val empty : 'a t
  val length : 'a t -> int
  val cons : 'a -> 'a t -> 'a t
  val decons : 'a t -> 'a*'a t
  val nth : 'a t -> int -> 'a
end 
		     
module EXE1 : RALIST with type 'a t = 'a list = 
struct 
  type 'a t = 'a list
  let empty = []
  let length l =
    List.length l
  let cons e l = e::l
  let decons l =
    match l with
    | [] -> assert false
    | one::rest -> (one, rest)
  let nth l n =
    List.nth l n
end

module EXE2 : RALIST = struct
  type 'a t = ('a list * int)
  let empty = ([],0)
  let length (_, n) = n
  let cons e (l,n) = (e::l, (n+1))
  let decons (l,n) = 
    match l with
    | e::rest -> e, (rest, (n-1))
    | _ -> assert false
  let nth (l, _) ith =
    List.nth l ith
end

module BList : RALIST = struct
  type 'a tree =
  | Leaf of 'a
  | Node of 'a tree * 'a tree

  let empty =
          let empty =  

end
