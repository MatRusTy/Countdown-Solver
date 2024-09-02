
let (let*) = Option.bind

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

let removei i lst = List.filteri (fun j -> fun _ -> i != j) lst

let mov1 lst1 lst2 : ('a list * 'a list) list =
  List.mapi (fun i -> fun x -> (removei i lst1, x :: lst2)) lst1

let mov2 lst1 lst2 : ('a list * 'a list) list =
  let mv1 = mov1 lst1 lst2 in
  List.flatten (List.map (fun (ls1, ls2) -> mov1 ls1 ls2) mv1)

let pick1 lst : ('a list * ('a option)) list =
  let choices = mov1 lst [] in
  List.map (fun (remaining, choice) -> 
      match choice with 
      | c1 :: _ -> (remaining, Some c1)
      | _ -> (remaining, None)
      ) choices

let pick2 lst : ('a list * ('a option * 'a option)) list =
  let choices1 = pick1 lst in
  let choices12 =
  List.map (fun (rem, c1) -> 
    let choices2 = pick1 rem in
    List.map (fun (rem, c2) -> (rem, (c1, c2))) choices2
    ) choices1
  in
  List.flatten choices12

let compose2 (ops : operator list) c1 c2 : computation list =
  List.filter_map (fun op ->
    let* newval = (op.op c1 c2) in
    let new_history = string_of_int c1 ^ op.print ^ string_of_int c2 ^ " = " ^ string_of_int newval ^ "\n" in
    Some { nums = [newval]; history = new_history; }
  ) ops

let combine_computations (cmp1 : computation) (cmp2 : computation) : computation =
  { nums = cmp1.nums @ cmp2.nums; history = cmp1.history ^ cmp2.history; }

let non_det_compose ops (cmp : computation) : computation list =
  let operand_choices = pick2 cmp.nums in
  List.flatten @@ List.filter_map (
    fun (remaining, (c1o, c2o)) ->
      let* c1 = c1o in
      let* c2 = c2o in
      let newCmps = compose2 ops c1 c2 in
      let currentCmp = { nums = remaining; history = cmp.history; } in
      Some (List.map (combine_computations currentCmp) newCmps)
    ) operand_choices

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
