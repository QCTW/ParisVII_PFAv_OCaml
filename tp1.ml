type 'a arbre =
  | Feuille of 'a
  | Node of ('a arbre) * ('a arbre)

let empty_blist = []

type 'a blist = ('a arbre * int) list

let rec consable a n bl =
  match bl with
  | [] -> [(a, n)]
  | (b, m)::bl2 -> 
    if n<m
    then (a,n)::bl
    else consable (Node(a, b)) (2*n) bl2

let rec deconsable bl =
  match bl with
  | [] -> []
  | b::bl2 -> bl2

let cons e bl = consable (Feuille e) 1 bl

let decons e bl = deconsable bl

let rec pop bl =
  match bl with
  | [] -> failwith "Empty list"
  | (b,n)::bl2 -> b

let rec nth n bl =
  let minus = n-1 in
  if minus=1
  then pop bl
  else 
    match bl with
    | [] -> failwith "Empty list"
    | b::bl2 -> nth minus bl2

let _ =
  let l = [] in
  let result = cons 33 l in
  Printf.printf "%d = %d" 1 (nth 1 result)

(*
let cons newEle myBList =
  let newF = Feuille(newEle) in
    match myBList with
    | h::[] -> Node(newF*h)::myBList
    | head::_ -> (
      match head with
      | Feuille a -> Node(newF*a)::myBList
      | Node _ -> newF::myBList
      )
    | [] -> newF::myBList

*)
