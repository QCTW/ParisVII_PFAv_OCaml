type color = R | B
type tree = E | T of color * tree * elem * tree

let rec member x = function
| E -> false
| T(_, a, y, b ) ->
if islt x y then member x a
else if islt y x then member x b
else true;;


