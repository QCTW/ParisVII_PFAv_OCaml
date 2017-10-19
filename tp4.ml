type 'a stack = 'a list
type 'a listzipper = 'a stack * 'a list

let mylist = [1;2;4;5]
let myzip = ([], mylist)

let rec gauche zip =
match zip with
| (s, h::tail) -> (h::s, tail)
| (_, []) -> ([], [])

let rec droite zip =
match zip with
| (h::tail, l) -> (tail, h::l)
| ([], l) -> ([], l)

let rec ajoute zip ele =
match zip with
| (s, l) -> (s, ele::l)

droite (droite (ajoute ( gauche ( gauche myzip) ) 3) )

type 'a arbre =
| Feuille
| Noeud of 'a * 'a arbre * 'a arbre
(** Means the direction of the exploration of the tree **)
type marqueur = Gauche | Droite
type 'a block = marqueur * 'a * 'a arbre
type 'a path = 'a block list
type 'a arbrezipper = 'a path * 'a arbre

let arbre_init = Noeud (5, Feuille, (Noeud (3, (Noeud (2, Feuille, Feuille)), (Noeud (4, Feuille, Feuille)) ) ) )

let myarbrezipper = ([], arbre_init)

let bas_a_gauche : 'a narbrezipper -> 'a nabrezipper zip =
function (pile, arbe) -> match pile zith
| (a::lp::l)
| (p, Noeud(v, arb_gauche, arb_droite)) -> ( (Gauche, v, arb_droite)::p, arb_gauche)
| (p, Feuille) -> (p, Feuille)

let bas_a_droite zip =
match zip with
| (p, Noeud(v, arb_gauche, arb_droite)) -> ( (Droite, v, arb_gauche)::p, arb_droite)
| (p, Feuille) -> (p, Feuille)

let en_haut zip =
match zip with
| (p, Noeud(v, arb_gauche, arb_droite)) -> 
(
match p with
| (Driote, ele, arb)::tail -> ...
| (Gauche, ele, arb)::tail -> ...Droite -> ( (Droite, v, arb_gauche)::p, arb_droite)
)
| (p, Feuille) -> (p, Feuille)


