open Expression

let insert_all_possible_indexes n ls =
	let rec insert_into list index = (match (list, index) with
		| (x :: xs, 0) -> n :: x :: xs
		| (x :: xs, i) -> x :: insert_into xs (i-1)
		| ([], _) -> [n]
	) in
	List.init ((List.length ls) + 1) (insert_into ls)

let rec all_shuffles_from ls = 
	match ls with
	| [] -> [[]]
	| x :: xs -> 
		let all_sl = all_shuffles_from xs in
		let all_sls_with_x = List.map (insert_all_possible_indexes x) all_sl in
		let liststs = List.flatten all_sls_with_x in
		liststs 

let op_to_target (list: int list) (target: int) : expr option = 
	let rec calculate runningVal runningExpr ls: expr option =
		match ls, runningVal with
		| _, i when i = target -> Some runningExpr
		| [], _ -> None
		| x :: xs, _ -> 
			let add_branch = calculate (runningVal + x) (Plus(runningExpr, Num x)) xs in
			if Option.is_some add_branch then
				add_branch
			else 
			let sub_branch = calculate (runningVal - x) (Minus(runningExpr, Num x)) xs in
			if Option.is_some sub_branch then
				sub_branch
			else
			let mul_branch = calculate (runningVal * x) (Times(runningExpr, Num x)) xs
			in
			if Option.is_some mul_branch then
				mul_branch
			else
			if runningVal mod x = 0 then
				calculate (runningVal / x) (Divide(runningExpr, Num x)) xs
			else
			None
	in
	calculate (List.hd list) (Num (List.hd list)) (List.tl list)

let rec find_first shuffles target : expr option =
	match shuffles with
	| [] -> None
	| shuffle :: remaining_shuffles -> 
		let attempt = op_to_target shuffle target in
		if Option.is_some attempt then
			Some (Option.get attempt)
		else
			find_first remaining_shuffles target

let solve input target: expr option = 
	let number_shuffles = all_shuffles_from input in
	find_first number_shuffles target
