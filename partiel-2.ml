type ('a, 'p) tree = 
| Empty
| Node of ('a * 'p) * ('a, 'p) tree * ('a, 'p) tree

(* Question 1 *)
(* Chaque noeud a une donnée ('a) et une priorité ('p). De plus, si un tel arbre est bien formé, alors 'p est croissante au sens qu'un père a une priorité inférieure que ses fils. En particulier, on peut extrait l'élément de priorité minimale en temps constant, car c'est la racine. *)

(* Question 2 *)
let is_empty tree = 
  tree = Empty

(* Question 3 *)
let rec push (label, priority) tree = 
  match tree with 
  | Empty -> Node((label, priority), Empty, Empty)
  | Node((x, p), left, right) -> 
    if(priority < p) then Node((label, priority), push (x,p) left, right)
    else Node((x,p), push (label, priority) left, right)

(* Question 4 *)
let peek tree =
  match tree with 
  | Empty -> failwith "File vide"
  | Node((x,p), left, right) -> x

(* Question 5 *)
let pop tree =
  let rec aux_merge t1 t2 =
  match t1, t2 with
    | Empty, t | t, Empty -> t
    | Node ((x1,p1), l1, r1), Node ((x2,p2), l2, r2) ->
      if(p1 < p2) then
        Node((x1,p1), l1, aux_merge t2 r1)
      else 
        Node((x2,p2), l2, aux_merge t1 r2)

  in match tree with 
  | Empty -> failwith "File vide"
  | Node((x,p), left, right) -> aux_merge left right


    


let () = 
  ();