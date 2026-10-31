type resultat = Note of int | Absence of bool
type bulletin = resultat list

let separer bulletin =
  let rec aux bul (notes, countJustified, countUnjustified) =
    match bul with
    | [] -> (List.rev notes, countJustified, countUnjustified)
    | head::tail -> match head with
      | Note note -> aux tail (note::notes, countJustified, countUnjustified)
      | Absence abs -> 
        if abs then aux tail (notes, countJustified+1, countUnjustified)
        else aux tail (notes, countJustified, countUnjustified+1)
  in aux bulletin ([],0,0)

let verifier_notes bul =
  let (notes, cJ, cUj) = separer bul in
  List.for_all (fun note -> (note >= 0 && note <= 20)) notes

let moyenne bul =
  let (notes, cJ, cUj) = separer bul in
  
  let rec sum notes (length, acc) =
    match notes with 
    | [] -> (length, acc)
    | head::tail -> sum tail (length+1, acc+head)
  in 
  let (gradesAmount, gradesSum) = sum notes (0,0) in
  if(gradesAmount + cUj = 0) then 0.0
  else float_of_int (gradesSum) /. float_of_int (gradesAmount + cUj)
  
(* Généralisation*)
type ('a, 'b) sum = Gauche of 'a | Droite of 'b

let convertir bulletin =
  let rec aux bul acc =
    match bul with 
    | [] -> List.rev acc
    | (Note note)::tail -> aux tail (Gauche note::acc)
    | (Absence abs)::tail -> aux tail (Droite abs::acc)
  in aux bulletin []

let rec filter_map f g l =
  match l with 
  | [] -> []
  | head::tail -> match head with
    | Gauche a -> (
      match (f a) with
      | None -> filter_map f g tail
      | Some c -> c :: filter_map f g tail  
    )
    | Droite b ->(
      match (g b) with
      | None -> filter_map f g tail
      | Some c -> c :: filter_map f g tail  
    )

let moyenne_bis bulletin =
  let bul_general = convertir bulletin in
  let notes_et_zero = filter_map (fun n -> Some n) (fun abs -> if abs then None else Some 0) bul_general in
  let somme_des_notes = List.fold_left (+) 0 notes_et_zero in
  let nombre_notes = List.length notes_et_zero in
  if(nombre_notes = 0) then nan
  else float_of_int somme_des_notes /. float_of_int nombre_notes

  let () = 
  let exemple = [Note 8; Absence true; Note 14; Note 16; Absence false; Absence true] in
  ()