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
	let cons x s = lazy (Cons (x,s))
	let get s = match s with lazy ( Cons (x,s') ) -> (x,s')
end

let rec take n s = 
	if n=0 then []
	else
		try
			let	(x,s') = MyStream.get s
			in x::take (n-1) s'
		with Empty -> []
		
let x = take 2 (MyStream.cons 3 MyStream.empty);;

type 'a lazy_state =
	| Delayed of (unit->'a)
	| Value of 'a
	| Exn of exn
;;

let create_lazy f = ref (Delayed f);;
let v = create_lazy (fun()->print_string "P lazy compute\n";sqrt 16.);;

(* let rec from n = 
	cons n (from(n+1)) *)