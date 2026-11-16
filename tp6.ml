(* Exercice 1 *)
let rec listn n =
  if n = 0 then [0]
  else n :: (listn (n - 1))

let listn_term n =
  let rec aux n acc =
    if n = 0 then 0::acc
    else aux (n-1) (n::acc)
  in aux n []

(* Exercice 2 *)
let rec length l = match l with
  | [] -> 0
  | _ :: l' -> 1 + (length l')

let length_term l =
  let rec aux l acc = match l with
    | [] -> acc
    | head::tail -> aux tail (acc+1)
  in aux l 0

(* Exercice 3 *)
let rev_append left right =
  let rec aux list acc =
    match list with 
    | [] -> acc
    | head::tail -> aux tail (head::acc)
  in aux left right

(* Exercice 4 *)
let exo4_append left right =
  rev_append (List.rev left) right
 
(* ====== PARTIE 2 ====== *)
type 'a tree =
| Nil
| Node of 'a * 'a tree * 'a tree

(* Exercice 5*)
let rec size tree = 
  match tree with
  | Nil -> 0
  | Node(tag, left, right) -> 1 + size left + size right
    
(* Exercice 6 *)
let rec size_aux list acc =
  match list with 
  | [] -> acc
  | head::tail -> match head with 
    | Nil -> size_aux tail acc
    | Node(tag, left, right) -> size_aux (left::right::tail) (1 + acc)

(* Exercice 8 *)
let size_ex8 tree =
  size_aux [tree] 0

(* ====== PARTIE 3 ====== *)


(* Exercice 9 *)
let insert x sorted_list =
  let rec aux x sList acc =
    match sList with
    | [] -> [x]
    | head::tail -> 
      if x > head then aux x tail (head::acc) 
      else if x = head then rev_append acc sList (* ne pas l'ajouter encore *)
      else rev_append (x::acc) sList
    in aux x sorted_list []

(* Exercice 10 *)
let sort list =
  let rec aux list acc =
    match list with 
    | [] -> acc
    | head::tail -> aux tail (insert head acc)
  in aux list []

(* Exercice 11 *)
let union_sorted leftSort rightSort =
  let rec aux leftSort acc =
    match leftSort with 
      | [] -> acc
      | head::tail -> aux tail (insert head acc)
  in aux leftSort rightSort

let rec mem_sorted sortedList x =
  match sortedList with 
  | [] -> false
  | head::tail -> (
    if(head = x) then true
    else if (head > x) then false
    else mem_sorted tail x
  )
  

let inter_sorted list1 list2 =
  let rec aux list1 list2 acc =
    match list1 with
    | [] -> List.rev acc
    | head::tail -> 
      if (mem_sorted list2 head) then aux tail list2 (head::acc)
      else aux tail list2 acc
    in aux list1 list2 []

(* Test *)
let print_int_list list = List.iter (fun v -> Printf.printf "%d, " v) list 

let () = 
  let l = [1;3;5] in
  let l2 = [2;5;8] in
  print_int_list (union_sorted l l2);
  print_int_list (inter_sorted l l2)