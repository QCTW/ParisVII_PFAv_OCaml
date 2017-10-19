module EnsDEntier = struct
  type t = int
  let compare = Pervasives.compare 
end

module IntSet = Set.Make(EnsDEntier)

let mySet = IntSet.add 4 (IntSet.add 3 (IntSet.add 2 (IntSet.add 1 IntSet.empty)))

let _ = IntSet.elements mySet

let powerset s = 
        IntSet.fold (fun e acc -> acc @ List.map (IntSet.add e) acc) s
        [IntSet.empty]

let _ = List.map IntSet.elements (powerset mySet)

module IntSetSet = Set.Make(IntSet)

let mySetSet = IntSetSet.add mySet IntSetSet.empty

let displaySS ss = List.map IntSet.elements (IntSetSet.elements ss)

let _ = displaySS mySetSet

let intsetset_map f ss =
  IntSetSet.fold (fun s acc -> IntSetSet.add (f s) acc) ss IntSetSet.empty

let powerset' s =
  IntSet.fold(fun x acc -> IntSetSet.union acc (intsetset_map (IntSet.add x)
  acc)) s (IntSetSet.singleton IntSet.empty)

let _ = displaySS (powerset' mySet)

module StrSet =
  Map.Make(struct type t = string let compare = compare end)

let assoc_via_map s m = StrSet.find s m

let db = StrSet.add "x" 3 (StrSet.add "y" 7 StrSet.empty)

let _ = assoc_via_map "y" db

module StrHash =
  Hashtbl.Make (struct
    type t = string
    let equal = (=)
    let hash = Hashtbl.hash
  end)

let assoc_via_hash s m = StrHash.find m s

let db' = StrHash.create 19

let () = StrHash.add db' "x" 3
let () = StrHash.add db' "y" 7


let _ = assoc_via_hash "y" db'

