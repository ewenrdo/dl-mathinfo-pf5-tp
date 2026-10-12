type formula =
| Prop of string
| Neg of formula
| And of formula * formula
| Or of formula * formula

(* Exercice 1 *)
let rec string_of_formula formula =
    match formula with
    | Prop f -> f  
    | Neg f -> "Neg " ^ string_of_formula f
    | And (f1,f2) -> "("^(string_of_formula f1) ^ " And " ^ (string_of_formula f2)^")"
    | Or (f1,f2) -> "("^(string_of_formula f1) ^ " Or " ^ (string_of_formula f2)^")"

(* Exercice 2 *)
let list_of_props formula =
    let rec aux formula acc =
        match formula with 
        | Prop f -> (f::acc)
        | Neg f ->  aux f acc
        | And (f1,f2) | Or(f1,f2)-> aux f2 (aux f1 acc)
    in List.sort_uniq compare (aux formula [])

(* Exercice 3 *)
(* Appelons interprétation toute liste d’association de type (string * bool) list.
Une interprétation permet de spécifier les valeurs de vérité des propositions d’une formule.
Chaque proposition apparaît au plus une fois dans la liste.
Pour la formule And (Prop "p", Prop "q"), par exemple, une interprétation possible est
[("p", true); ("q", false)].

Écrire une fonction eval_formula : formula -> (string * bool) list -> bool calculant
la valeur de vérité d’une formule relativement à une interprétation donnée.
Par exemple eval_formula (And (Prop "p", Prop "q")) [("p", true); ("q", false)] don-
nera false.
Vous pouvez utiliser List.assoc v l qui renvoie la partie droite b du premier couple (v, b)
dans la liste l, et déclenche l’exception Not_found si un tel couple n’existe pas.
*)

let rec eval_formula formula interpretation = 
    match formula with
    | Prop f -> List.assoc f interpretation
    | Neg f -> not (eval_formula f interpretation)
    | And (f1,f2) -> (eval_formula f1 interpretation) && (eval_formula f2 interpretation)
    | Or (f1,f2) -> (eval_formula f1 interpretation) || (eval_formula f2 interpretation)

(* Exercice 4 *)
let add_to_all_manual x list =
    let rec aux x list acc =
        match list with 
        | [] -> List.rev acc
        | head::tail -> aux x tail ((x::head)::acc)
    in aux x list []

let add_to_all x list = List.map (fun l -> x::l) list

(* Exercice 5 *)
let rec interpretations_props props =
    match props with
    | [] -> [[]]
    | head::tail -> 
        let rest = interpretations_props tail in
        (add_to_all (head, true) rest) @ (add_to_all (head, false) rest)

let interpretations formula = interpretations_props (list_of_props formula)

(* Exercice 6 *)
let satisfiable formula =
    let interpretations_of_props = interpretations formula in
    List.exists (fun interpretation -> eval_formula formula interpretation) interpretations_of_props
    
let totology formula =
    let interpretations_of_props = interpretations formula in
    List.for_all (fun interpretation -> eval_formula formula interpretation) interpretations_of_props
    
(* Exercice 7 *)
let is_consequence formula1 formula2 =
  let propsF = list_of_props formula1 in 
  let propsG = list_of_props formula2 in
  let data_list_union = List.sort_uniq compare (propsF @ propsG) in
    let data_list = interpretations_props data_list_union in
        let rec aux f g data_list =
            match data_list with
            | [] -> true
            | head::tail -> 
                let evalFOnData = eval_formula f head in
                if(evalFOnData && not (eval_formula g head)) then false
                else aux f g tail
        in aux formula1 formula2 data_list
  
let is_equivalent formula1 formula2 =
    let propsF = list_of_props formula1 in 
    let propsG = list_of_props formula2 in
    let data_list_union = List.sort_uniq compare (propsF @ propsG) in
    let data_list = interpretations_props data_list_union in
        let rec aux f g data_list =
            match data_list with
            | [] -> true
            | head::tail -> 
                let evalFOnData = eval_formula f head in
                let evalGOnData = eval_formula g head in
                if(evalFOnData = evalGOnData) then aux f g tail
                else false
        in aux formula1 formula2 data_list

let is_equivalent_bis f g = (is_consequence f g) && (is_consequence g f)
    
let rec desc_neg formula =
    match formula with 
    | Prop f -> Prop f
    
    | Neg(Prop f) -> Neg(Prop f)
    | Neg(Neg f) -> desc_neg f
    | Neg(And(f1,f2)) -> Or(desc_neg (Neg f1), desc_neg (Neg f2))
    | Neg(Or(f1,f2)) -> And(desc_neg (Neg f1), desc_neg (Neg f2))


    | And(f1,f2) -> And(desc_neg f1,desc_neg f2)
    | Or(f1,f2) -> Or(desc_neg f1, desc_neg f2)

let rec desc_or formula =
    match formula with
    | Prop f -> Prop f
    | And(f1,f2) -> And(desc_or f1,desc_or f2)
    | Neg f -> Neg (desc_or f)
    | Or(f1,And(f2,f3)) -> desc_or (And(Or(f1, f2), Or(f1, f3)))
    | Or(And(f1,f2),f3) -> desc_or (And(Or(f1, f3), Or(f2, f3)))

    | Or(f1,f2) -> Or(desc_or f1, desc_or f2) (* ne fonctionne pas *)
    
let rec desc_or_CORRIGE formula =
  match formula with
  | Prop f -> Prop f
  | Neg f -> Neg (desc_or f)
  | And (f1, f2) -> And (desc_or f1, desc_or f2)
  | Or (f1, And (f2, f3)) ->
      desc_or (And (Or (f1, f2), Or (f1, f3)))
  | Or (And (f1, f2), f3) ->
      desc_or (And (Or (f1, f3), Or (f2, f3)))
  | Or (f1, f2) ->
      let f1' = desc_or f1 in
      let f2' = desc_or f2 in
      if f1' <> f1 || f2' <> f2 then desc_or (Or (f1', f2'))
      else Or (f1', f2')


(* Tests *)

let rec print_string_list list =
  match list with
  | [] -> print_newline ()
  | head::tail -> print_string(head); print_string ","; print_string_list(tail)


let print_bool boolean = 
    if boolean then print_string "True"
    else print_string "False"

let () = 
    let formulaTest = And (Prop "p", Neg (Or (Prop "q", Prop "r"))) in
    print_string_list (list_of_props formulaTest);
    print_bool (eval_formula (And (Prop "p", Prop "q")) [("p", true); ("q", false)])