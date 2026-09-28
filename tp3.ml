(* Exercice  *)
let rec get list i =
  match list with
  | [] -> failwith "Index out of range"
  | head::rest -> if i = 0 then head else get rest (i-1)

let choose list =
  let listLength =  List.length list in
  let random = Random.int listLength in
  get list random

(* Exercice 2 *)
let rec mem x list =
  match list with
  | [] -> false
  | head::tail -> head = x || mem x tail

let choose_elements list n =
  let rec choose_elements_aux l n acc alreadySort = 
    if(n=0) then acc
    else 
      let position = Random.int (List.length l) in
      if(mem position alreadySort) then choose_elements_aux l n acc alreadySort
      else choose_elements_aux l (n-1) ((get l position)::acc) (position::alreadySort)
    in choose_elements_aux list n [] []

(* Exercice 3 *)
let choose_sublist list n =
  let rec choose_sublist_aux list remaining acc listLength =
    match list with 
    | [] -> List.rev acc
    | head::tail -> (
      let probTriage = Random.float 1.0 in
      if(probTriage < (float_of_int remaining) /. (float_of_int listLength)) then choose_sublist_aux tail (remaining - 1) (head::acc) (listLength-1)
      else choose_sublist_aux tail remaining acc (listLength-1)
    )
    in choose_sublist_aux list n [] (List.length list)

(* Exercice 4 *)
let insert list x = 
  let rec aux sortedList x acc =
    match sortedList with 
    | [] -> List.rev acc @ [x]
    | head::tail -> (
      if head = x then (List.rev acc) @ sortedList
      else if head > x then (List.rev acc) @ (x::head::tail)
      else aux tail x (head::acc)
    )
    in aux list x []

(* Exercice 5 *)
let sort list =
  let rec aux list acc =
    match list with 
    | [] -> acc
    | head::tail -> aux tail (insert acc head)
  in aux list []

(* Exercice 6 *)
let rec mem_sorted sortedList x =
  match sortedList with 
  | [] -> false
  | head::tail -> (
    if(head = x) then true
    else if (head > x) then false
    else mem_sorted tail x
  )

(* Exercice 7 *)

let rec insert_all_sorted list acc = 
  match list with
  | [] -> acc
  | head::tail -> insert_all_sorted tail (insert acc head) 


let rec union_sorted acc list2 =
  match list2 with
  | [] -> acc
  | head::tail -> union_sorted (insert acc head) tail

let rec inter_sorted list1 list2 acc =
  match list1 with
  | [] -> List.rev acc
  | head::tail -> 
    if (mem_sorted list2 head) then inter_sorted tail list2 (head::acc)
    else inter_sorted tail list2 acc

(* Exercice 8 *)
let partition pivot list =
  let rec partition_aux pivot list inf sup = (* inf et sup sont nos deux ensembles formant la partition*)
    match list with
    | [] -> (List.rev inf, List.rev sup)
    | head::tail -> (
      if pivot < head then (partition_aux pivot tail inf (head::sup))
      else partition_aux pivot tail (head::inf) sup
    )
    in partition_aux pivot list [] [] 

let rec quicksort list =
  match list with
  | [] -> []
  | pivot::tail -> (
    let (inf, sup) = partition pivot tail in
     ((quicksort inf) @ [pivot]) @ (quicksort sup) 
  )

(* Tests *)
 
let rec print_int_list list =
  match list with
  | [] -> print_newline ()
  | head::tail -> print_int(head); print_string ","; print_int_list(tail)


let () = 
    Random.self_init ();
    let list1 = [1;3;5] in
    let list2 = [2;5;8] in
    print_int_list (inter_sorted list1 list2 []);
    