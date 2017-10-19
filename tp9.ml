type value = Int of int | Float of float
let nombres = [Float 2.5; Int 1; Float 3.14]

type _ kind =
 | SomeInt : int kind
 | SomeFloat : float kind

(*
type 'a kind =
 | SomeInt : int kind
 | SomeFloat : float kind
*)

let rec get: type a.a kind -> value list -> a =
  fun k l ->
    match (k, l) with
    | SomeInt, Int n::_ -> n
    | SomeFloat, Float f::_ -> f
    | _, [] -> failwith "Empty list"
    | _, _::lrest -> get k lrest

let i = get SomeInt nombres

let f = get SomeFloat nombres

let _ = 
Printf.printf "SomeInt=%d\n" i;
Printf.printf "SomeFloat=%f\n" f


type _ format =
 | Nop : unit format
 | Int : 'a format -> (int -> 'a) format
 | Float : 'a format -> (float -> 'a) format

let rec printf: type a.a format -> a =
 function
 | Nop -> ()
 | Int ft -> fun n -> print_int n; print_char '\n'; printf ft
 | Float ft -> fun f -> print_float f; print_char '\n'; printf ft

type _ kind2 =
 | Int : int kind2
 | Float : float kind2
 | List : 'a kind2 -> ('a list) kind2
 | Pair : 'a kind2 * 'b kind2 -> ('a * 'b) kind2

type _ format2 =
 | Nop : unit format2
 | Seq : 'a kind2 * 'b format2 -> ('a -> 'b) format2


let _ =
  printf (Int(Float(Nop))) 2 2.0
