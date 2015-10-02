let somme x y = x + y;;
(* val somme : int -> int -> int = <fun> *)
somme (somme (somme 2 3) 4) (somme 2 (somme 3 4));;
(* - : int = 18 *)
