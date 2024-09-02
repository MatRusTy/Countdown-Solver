open Expression
open Interpreter

let non_det_compose (elem : 'A) (op : 'A -> 'A -> 'A) (elems : 'A list) : 'A list =
  List.map (op elem) elems

let rec non_det_compute (elems : 'A list) (ops : ('A -> 'A -> 'A) list) : 'A list =
  match elems with
  | [] -> []
  | [x] -> [x]
  | x :: xs ->
      let sub_elems = non_det_compute xs ops in
      List.flatten (List.map (fun op -> non_det_compose x op sub_elems) ops)


let rec insert (x : 'A) (lst : 'A list) : 'A list list =
  match lst with
  | [] -> [[x]]
  | h :: t -> 
      (x :: lst) :: (List.map (fun el -> h :: el) (insert x t))

let rec permutations (lst : 'A list) : 'A list list =
  match lst with
  | [] -> [[]]
  | h :: t -> 
      List.flatten (List.map (insert h) (permutations t))

let rec powerset (lst : 'A list) : 'A list list = 
  match lst with
  | [] -> [[]]
  | x :: xs ->
      let ps = powerset xs in
      ps @ List.map (fun ss -> x :: ss) ps

let solve input target: expr list = 

  let input_exprs = List.map (fun x -> Num x) input in

  let input_perms = 
    List.flatten (List.map (fun lst -> permutations lst) (powerset input_exprs))
  in

  let add = fun x y -> Plus (x, y) in
  let sub = fun x y -> Minus (x, y) in
  let mul = fun x y -> Times (x, y) in
  let div = fun x y -> Divide (x, y) in

  let ops = [add; sub; mul; div] in

  let all_expr =
    List.flatten (List.map (fun perm -> non_det_compute perm ops) input_perms)
  in

  List.filter (fun e -> eval e = Some target) all_expr
  