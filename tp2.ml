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

let rec print_int_list list =
  match list with
  | [] -> print_newline ()
  | head::tail -> print_int(head); print_string ","; print_int_list(tail)

let append list1 list2 =
  let rec aux list1 list2 acc =
    match list1 with 
    | []-> (match list2 with 
        | [] -> List.rev acc
        | head2::tail2 -> aux list1 tail2 (head2::acc))
    | head::tail -> aux tail list2 (head::acc)
  in aux list1 list2 []



let () = 
  let list1 = [7;2;3;1] in
  let list2 = [9;6;2;3] in

  print_int_list (append list1 list2)