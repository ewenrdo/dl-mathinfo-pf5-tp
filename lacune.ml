type ('a, 'b) sum = 
  | Left of 'a 
  | Right of 'b

let extraire_int sum =
  match sum with
  | Left l -> Some l
  | Right r -> None

let rec filter_map f l =
  match l with 
  | [] -> []
  | head::tail -> 
    match f head with
    | None -> filter_map f tail
    | Some v -> v :: (filter_map f tail)

let rec fold_left f acc l =
  match l with
  | [] -> acc
  | head::tail -> fold_left f (f acc head) tail


let () = 
  let list = [Left 42; Right "hello"] in
  let int = extraire_int (Left 42) in
  ();