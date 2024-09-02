open NonDeterministicComputations

let rec powerset lst =
  match lst with
  | [] -> [[]]
  | x :: xs ->
     let ps = powerset xs in
     ps @ List.map (fun ss -> x :: ss) ps

(* Operations used in regular countdown *)

let add_op : int operator =
  {
    op = (fun x -> fun y ->
      let newval = x.elem + y.elem in
      Some
      {
        elem = newval;
        print = string_of_int newval;
        history = x.history ^ y.history ^ x.print ^ " + " ^ y.print ^ " = " ^ string_of_int newval ^ "\n";
      }
      );
    print = "+";
  }

let sub_op : int operator =
  {
    op = (fun x -> fun y ->
      let newval = x.elem - y.elem in
      if newval < 0 then None else
      Some
      {
        elem = newval;
        print = string_of_int newval;
        history = x.history ^ y.history ^ x.print ^ " - " ^ y.print ^ " = " ^ string_of_int newval ^ "\n";
      }
      );
    print = "-";
  }

let mul_op : int operator =
  {
    op = (fun x -> fun y ->
      let newval = x.elem * y.elem in
      Some
      {
        elem = newval;
        print = string_of_int newval;
        history = x.history ^ y.history ^ x.print ^ " * " ^ y.print ^ " = " ^ string_of_int newval ^ "\n";
      }
      );
    print = "*";
  }

let div_op : int operator =
  {
    op = (fun x -> fun y ->
      if y.elem = 0 || x.elem mod y.elem != 0 then None else
      let newval = x.elem / y.elem in
      Some
      {
        elem = newval;
        print = string_of_int newval;
        history = x.history ^ y.history ^ x.print ^ " / " ^ y.print ^ " = " ^ string_of_int newval ^ "\n";
      }
      );
    print = "/";
  }

let solve (input : int list) (target : int) : int result list =
  let ops = [ add_op; sub_op; mul_op; div_op ] in

  let operands_seed = List.map (fun i -> 
      {
        elem = i;
        print = string_of_int i;
        history = "";
      }
    ) input
  in

  let operand_choices = powerset operands_seed in

  List.flatten @@ List.map (fun operand_choice ->
    let results = compute_non_det ops operand_choice in
    List.filter (fun res -> res.res = target) results
  ) operand_choices
