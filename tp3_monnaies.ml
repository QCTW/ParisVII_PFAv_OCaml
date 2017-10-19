module MFloat = struct
  type t = float
  let un = 1.0
  let plus = (+.)
  let prod = ( *. )
end

module type MONNAIE = sig 
  type t
  val un : t
  val plus : t -> t -> t
  val prod : float -> t -> t
end

module Euro = (MFloat : MONNAIE)
module Dollar = (MFloat : MONNAIE)

let euro x = Euro.prod x Euro.un;;

(*Euro.plus (euro 50.0) (Dollar.un);; *)

Euro.plus (euro 10.0) (euro 20.0);;

module BuDeChange : sig
  module Euro : MONNAIE
  module Dollar : MONNAIE
  val euro_to_dollar : Euro.t -> Dollar.t
  val dollar_to_euro : Dollar.t -> Euro.t
end = struct
  module Euro = MFloat
  module Dollar = MFloat
  let un_euro_en_dollar = 0.95
  let euro_to_dollar e =
          e /. un_euro_en_dollar
  let dollar_to_euro d =
          d *. un_euro_en_dollar
end;;

module Euro = BuDeChange.Euro;;
module Dollar = BuDeChange.Dollar;;

let deux_euro = Euro.prod 2.0 Euro.un

let deux_euro_en_dollar = BuDeChange.euro_to_dollar deux_euro

let _ = Printf.printf deux_euro_en_dollar
