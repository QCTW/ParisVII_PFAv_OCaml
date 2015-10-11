module Mfloat =
	struct
		type t = float
		let un = 1.0
		let plus x y = x +. y
		let prod x y = x * y
	end;;

(* module type == interface *)
module type MONNAIE =
	sig
		type t : float
		val un : t
		val plus : t -> t -> t
		let prod : float -> t -> t
	end;;

module Euro = (MFloat : MONNAIE);;
module Dollar = (MFloat : MONNAIE);;