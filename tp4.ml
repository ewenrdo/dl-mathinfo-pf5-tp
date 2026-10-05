type 'a tree = 
| Nil 
| Node of 'a * 'a tree * 'a tree

(* Exercice 1 *)
let rec size root =
    match root with 
    | Nil -> 0
    | Node(head, right, left) -> 1 + size left + size right

let rec depth root = 
  match root with
  | Nil -> 0
  | Node(head, right, left) -> 1 + max (depth right) (depth left)

let rec sum root =
  match root with
  | Nil -> 0
  | Node(head, right, left) -> head + sum right + sum left

let rec contains root x =
  match root with
  | Nil -> false
  | Node(head, right, left) -> if head = x then true else contains right x || contains left x

let rec elements root =
  match root with 
  | Nil -> []
  | Node (node, left, right) -> (elements left) @ [node] @ (elements right)

let rec pow x n =
  if(n = 0) then 1
  else x * pow x (n-1)

let perfect root =
  match root with
  | Nil -> true
  | Node (node, left, right) -> (pow 2 (depth root)) - 1= size root 

let rec contains_bst root x =
  match root with 
  | Nil -> false
  | Node(node, left, right) -> if node = x then true else if node < x then contains_bst right x else contains_bst left x 

let rec add_bst root x =
  match root with 
  | Nil -> Node(x, Nil, Nil)
  | Node(node, left, right) -> 
    if node = x then Node(node, left, right) 
    else if node > x then Node(node, add_bst left x, right)
    else Node(node, left, add_bst right x)

let bst_of_list list = 
  let rec aux list acc =
    match list with 
    | [] -> Nil
    | head::tail -> aux tail (add_bst acc head)
  in aux list Nil (* un arbre filliforme si la liste des triée, c'est chiant pour les opérations. *)

let rec split_at list k =
  if k = 0 then ([], list)
  else
    match list with
    | [] -> ([], [])
    | head :: tail ->
        let (gauche, droite) = split_at tail (k - 1) in
        (head :: gauche, droite)

let rec bst_of_list_opt list listLength =
  if(listLength < 0) then failwith "La longueur de la liste est invalide"
  else 
    let k = listLength / 2 in
    let (gauche, reste) = split_at list k
    in match reste with 
    | [] -> Nil
    | median :: droite -> Node(median, bst_of_list_opt gauche k, bst_of_list_opt droite (listLength - k-1)) 

let rec forall_labels p tree =
  match tree with
  | Nil -> true
  | Node(tag, left, right) -> p tag && forall_labels p left && forall_labels p right

let is_uniform v tree = 
  let predicate tag = (tag = v) in
  forall_labels predicate tree

let rec forall_subtree p tree =
  match tree with
  | Nil -> true
  | Node(tag, left, right) -> p tag left right && forall_subtree p left && forall_subtree p right

let is_right_comb tree =
  let p _ left _ = (left = Nil) in
  forall_subtree p tree

let rec fold_tree f defaultValue tree =
  match tree with
  | Nil -> defaultValue
  | Node(n, g, d) -> f n (fold_tree f defaultValue g) (fold_tree f defaultValue d)

let sum tree = 
  let f n left right = n + left + right 
  in fold_tree f 0 tree

(*
let rec map_tree f tree =
  match tree with 
  | Nil -> Nil
  | Node(tag, left, right) -> (f tag, map_tree f left, map_tree f right)
*)

let map_tree_bis f tree =
  let fn tag left right = Node(f tag, left, right)
  in fold_tree fn Nil tree