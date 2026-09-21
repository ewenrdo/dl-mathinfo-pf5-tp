(* Exercice 1 *)
(*
a. let e = 4 in (e, "a")::[] --> int * string list
b. (fun x y -> x y) (fun l -> 1::l) [4; 8] --> ??
c. fun l -> match l with [] -> 111 | a::_ --> a' list -> a
*)

(* Exercice 2 *)
let rec length list =
  match list with
  | [] -> 0 
  | _::rest -> 1 + length rest
  
let rec list_sigma list =
  match list with
  | [] -> 0
  | head::rest -> head + (list_sigma rest)

let append list1 list2 =
  let rec aux list1 list2 acc =
    match list1 with 
    | []-> (match list2 with 
        | [] -> List.rev acc
        | head2::tail2 -> aux [] tail2 (head2::acc))
    | head::tail -> aux tail list2 (head::acc)
  in aux list1 list2 []

let split list =
  let rec aux list acc =
    match list with
    | [] -> (List.rev (fst acc), List.rev(snd acc))
    | head::tail -> aux tail ((fst head)::fst acc, (snd head)::snd acc)
  in aux list ([],[])


let rev list =
  let rec aux list acc =
    match list with
    | [] -> acc
    | head::tail -> aux tail (head::acc)
  in aux list []

let flatten list =
  let rec aux list acc =
    match list with
    | [] -> acc
    | head::tail -> aux tail (append head acc)
  in aux (rev list) []
(* on aurait pu faire acc @ head, mais c'est pas opti, là, on est en O(N). *)

let rec mem x list =
  match list with
  | [] -> false
  | head::tail -> head = x || mem x tail

let simplify list =
  let rec aux list acc =
    match list with
    | [] -> rev acc
    | head::tail -> if (mem head acc) then aux tail acc else aux tail (head::acc)
  in aux list []

let successors x list =
  let rec aux x list acc = 
    match list with 
    | [] -> rev acc
    | [_] -> rev acc (* 1 seul élément, équivalent à head::[] *)
    | head::succ::tail -> if head = x then (aux x (succ::tail) (succ::acc)) else (aux x (succ::tail) acc)
  in aux x list []

let list_min list =
    match list with 
    | [] -> failwith "Empty list doesn't have a min"
    | head::tail -> let rec aux list acc =
      match list with
      | [] -> acc
      | head::tail -> aux tail (min head acc)
    in aux list head

(* Exercice 3 *)
let map f list =
  let rec aux f list acc =
    match list with 
    | [] -> rev acc
    | head::tail -> aux f tail ((f head)::acc)
  in aux f list []

let rec exists p list =
  match list with
  | [] -> false
  | head::tail -> (p head) || exists p tail
let rec for_all p list =
  match list with
  | [] -> true
  | head::tail -> (p head) && for_all p tail

let filter f list =
  let rec aux f list acc =
    match list with
    | [] -> rev acc
    | head::tail -> if f head then aux f tail (head::acc) else aux f tail acc
  in aux f list []

(* Commandes utiles *)
let rec print_int_list list =
  match list with
  | [] -> print_newline ()
  | head::tail -> print_int(head); print_string ","; print_int_list(tail)

let rec print_char_list list =
  match list with
  | [] -> print_newline ()
  | head::tail -> print_char(head); print_string ","; print_char_list(tail)

(* Tests *)
let () = 

  let list = [1;1;2;9;2;1;4;7;6] in
  print_int (list_min list);
  print_newline ();
