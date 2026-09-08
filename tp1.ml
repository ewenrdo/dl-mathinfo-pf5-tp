let square x = x * x

(* Exercice 4 *)

(* Exercice 5 *)
let perimeter radius = 
  let math_pi = 3.1415927
  in  2.0 *. math_pi *. radius

(* Exercice 6 *)
let div n m = (float_of_int n) /. (float_of_int m)
  (* 0/0 -> NaN, x / 0 -> infini (inf) *)

(* Exercice 7*)
let bis string = string ^ string

(* Exercice 8*)
(* Exercice 8 : Écrire une fonction times8 attendant une chaîne de caractères et renvoyant huit
exemplaires de cette chaîne concaténés à la suite. On notera que :
– en concaténant "ab" à elle-même, on obtient "abab" (deux fois "ab")
– en concaténant "abab" à elle-même, on obtient "abababab" (quatre fois "ab")
– en concaténant "abababab" à elle-même, on obtient "abababababababab" (huit fois "ab")
Servez-vous de cette propriété pour construire le résultat de cette fonction à l’aide d’une suite
de définitions locales.*)
let times8 string =
  let string2 = string ^ string in
  let string4 = string2 ^ string2 in
  string4 ^ string4

(* Exercice 9 *)
let times8_bis string = bis (bis (bis string))

(* Exercice 10 *)
let is_zero n = (n = 0)

(* Exercice 11 *)
let msg_zero n = if is_zero n then "zero" else "not zero"

(* Exercice 12 *)
let max a b = if a > b then a else b
let max_triple a b c = max (max a b) c
let max_quadruple a b c d = max (max_triple a b c) d 

(* Exercice 13 *)
let rec sum n = 
  if(n<0) then failwith "n must be a positive value" (*si pas ça, StackOverflow*)
  else if(n = 0) then 0
  else n + sum (n-1)

(* Exercice 14 *)
let rec fiboStyle n =
  if(n<0) then failwith "n must be a positive value" (*si pas ça, StackOverflow*)
  else if(n = 0 || n=1) then 1
  else (fiboStyle (n-1)) + (fiboStyle (n-2))

(* Exercice 15 *)
let rec composer f n =
  if(n<0) then failwith "n must be a positive value" (*si pas ça, StackOverflow*)
  else if(n=0) then f(0)
  else composer f (n-1) + f(n)

let sum_square n = composer square n

(* exercice 16 *)
let rec binome p n =
  if(p>n) then 0
  else if p = 0 then 1
  else binome p (n-1) + binome (p-1) (n-1) 

(* Exercice 17 *)
let () = 
  print_int (max_int);
  print_newline ();
  print_int (int_of_float (float_of_int max_int)); (* négatif *)
  print_newline ();