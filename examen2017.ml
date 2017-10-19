module type ORDER = sig
  type t
  val leq : t -> t -> bool
  val equal : t -> t -> bool
end

module Int:ORDER with type t = int = struct
  type t = int (* the t here and the t in "with type t" must be the same "set of type" *)
  let leq = (<=)
  let equal = (=)
end

type 'a t = P of 'a

let q = P(1)

type 'a t = 'a list

let q : ('a t) = [1; 2; 3]

module type INTERVAL = sig
  exception NoOverlap
  type element
  type t
  val create : element -> element -> t
  val equal : t -> t -> bool
  val mem : element -> t -> bool
  val before : t -> t -> bool
  val intersection : t -> t -> t
  val union : t -> t -> t
end

module type X_int = sig val x : int end

module Increment (M : X_int) : X_int = struct
  let x = M.x + 1
end

module Inc : X_int = struct
 
end
