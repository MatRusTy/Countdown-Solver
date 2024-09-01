open Interpreter

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

type computation =
{
  nums : int list;
  history : string;
}

type result =
{
  res : int;
  history : string;
}

type operator =
{
  op : int -> int -> int option;
  print : string;
}


let pick2 (lst : computation) : computation list =
List.map (fun p ->
{
  nums = p;
  history = lst.history;
})
(permutations lst.nums) (* Todo: optimise *)

let comp2 (ops : operator list) (cmp : computation): computation list =
  match cmp.nums with
  | [] -> [cmp]
  | [_] -> [cmp]
  | x :: y :: lst' -> List.filter_map (fun op ->
      let* newval = (op.op x y) in
      Some
      {
        nums = newval :: lst';
        history = cmp.history ^ string_of_int x ^ op.print ^ string_of_int y ^ " = " ^ string_of_int newval ^ "\n";
      }
    ) ops

let non_det_compose ops lst =
  List.flatten (List.map (comp2 ops) (pick2 lst))


let rec non_det_compute_inner (ops) (cmps : computation list) (ress : result list) : result list =
  match cmps with
  | [] -> ress
  | cmp :: lsts' -> 
      match cmp.nums with
      | [] -> ress
      | [x] -> non_det_compute_inner ops lsts' ({res = x; history = cmp.history} :: ress)
      | _ -> non_det_compute_inner ops ((non_det_compose ops cmp) @ lsts') ress

let non_det_compute ops (lst : computation) : result list =
  non_det_compute_inner ops [lst] []

let solve input target: result list =
  let ops =
    [
      {
        op = (fun x -> fun y -> Some (x + y));
        print = "+";
      };
      {
        op = (fun x -> fun y -> if x - y >= 0 then Some (x - y) else None);
        print = "-";
      };
      {
        op = (fun x -> fun y -> Some (x * y));
        print = "*";
      };
      {
        op = (fun x -> fun y -> if y != 0 && x mod y = 0 then Some (x / y) else None);
        print = "/";
      };
    ]
  in

  let computation_seed = {nums = input; history = ""} in

  let results = non_det_compute ops computation_seed in
  
  List.filter (fun res -> res.res = target) results
