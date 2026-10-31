type br_tree = 
| Nil 
| B of br_tree * br_tree 
| R of br_tree * br_tree

(* Question 1 *)
let rec count_b root =
  match root with
  | Nil -> 0
  | R (left, right) -> (count_b left) + (count_b right)
  | B (left, right) -> 1 + (count_b left) + (count_b right)

(* Question 2*)
let rec all_r root =
  match root with
  | Nil -> true
  | R (left, right) -> all_r left && all_r right
  | B (left, right) -> false

(* Question 3 *)
let is_correct root =
  let rec aux root b_acc r_acc =
      match root with
      | Nil -> b_acc = r_acc
      | R(left, right) -> aux left b_acc (r_acc+1) && aux right b_acc (r_acc+1)
      | B(left, right) -> aux left (b_acc+1) r_acc && aux right (b_acc+1) r_acc
  in aux root 0 0

(* Question 4 *)
let rec fold_br fNil fB fR t =
  match t with 
  | Nil -> fNil
  | B (l,r) -> fB (fold_br fNil fB fR l) (fold_br fNil fB fR r)
  | R (l,r) -> fR (fold_br fNil fB fR l) (fold_br fNil fB fR r)


let rec count_b_bis root =
  fold_br 0 (fun lNbr rNbr -> 1 +lNbr + rNbr) (fun lNbr rNbr -> lNbr + rNbr) root

let rec all_r_bis root =
  fold_br true (fun resLeft resRight -> false) (fun resLeft resRight -> resLeft && resRight) root

(* Question 5 *)
let aux = fold_br [0] (fun x y -> List.map (fun n -> n+1) (x@y)) (fun x y -> List.map (fun n -> n-1) (x@y))

let is_correct_bis root =
  List.for_all (fun n -> n = 0) (aux root)

(* Question *)

let () = 
  print_int (if all_r (B (R (Nil, B(Nil,Nil)),Nil)) then 1 else 0);
  print_int (if all_r (R (R (Nil, R(Nil,Nil)),Nil)) then 1 else 0)
