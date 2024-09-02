
let (let*) = Option.bind

type 'a operand =
{
  elem : 'a;
  print : string;
  history : string;
}

type 'a operator =
{
  op : 'a operand -> 'a operand -> 'a operand option;
  print : string;
}

type 'a computation = 'a operand list

type 'a result =
{
  res : 'a;
  history : string;
}

let operand_to_result opr =
  {
    res = opr.elem;
    history = opr.history ^ "\n---------------------\n"
  }

let removei i lst = List.filteri (fun j -> fun _ -> i != j) lst

let mov1_non_det lst1 lst2 : ('a list * 'a list) list =
  List.mapi (fun i -> fun x -> (removei i lst1, x :: lst2)) lst1

let pick1_non_det lst : ('a list * ('a option)) list =
  let choices = mov1_non_det lst [] in
  List.map (fun (remaining, choice) -> 
      match choice with 
      | c1 :: _ -> (remaining, Some c1)
      | _ -> (remaining, None)
      ) choices

let pick2_non_det lst : ('a list * ('a option * 'a option)) list =
  let choices1 = pick1_non_det lst in
  let choices12 =
  List.map (fun (rem, c1) -> 
    let choices2 = pick1_non_det rem in
    List.map (fun (rem, c2) -> (rem, (c1, c2))) choices2
    ) choices1
  in
  List.flatten choices12

let compose_non_det (ops : 'a operator list) opr1 opr2 : 'a operand list =
  List.filter_map (fun op ->
    let* newval = (op.op opr1 opr2) in
    Some newval
  ) ops

let pick_and_compose_non_det ops (cmp : 'a computation) : 'a computation list =
  let operand_choices = pick2_non_det cmp in
  List.flatten @@ List.filter_map (
    fun (remaining, (opr1o, opr2o)) ->
      let* opr1 = opr1o in
      let* opr2 = opr2o in
      let newOperands = compose_non_det ops opr1 opr2 in
      Some (List.map (fun opr -> opr :: remaining) newOperands)
    ) operand_choices

let compute_non_det ops (cmp : 'a computation) : 'a result list =
  let rec compute_non_det_inner ops (cmps : 'a computation list) (res_acc : 'a result list) : 'a result list =
    match cmps with
    | [] -> res_acc
    | cmp :: cmps' ->
        match cmp with
        | [] -> res_acc
        | [x] -> compute_non_det_inner ops cmps' ((operand_to_result x) :: res_acc)
        | _ -> compute_non_det_inner ops ((pick_and_compose_non_det ops cmp) @ cmps') res_acc
  in
  compute_non_det_inner ops [cmp] []
