let x = 2;;
(* val x : int = 2 *)
let x = 3 in 
let y = x + 1 in 
x + y;;
(* - : int = 7 *)
let x = 3 and y = x + 1 in
x + y;;
(* - : int = 6 parce que ici x=val=2, pas 3! donc 2+3+1=6*)