
type expr =
	| Num of int
    | Plus of expr * expr        (* a + b *)
    | Minus of expr * expr       (* a - b *)
    | Times of expr * expr       (* a * b *)
    | Divide of expr * expr      (* a / b *)


let rec to_string e =
    match e with
	| Num i -> string_of_int i
    | Plus (left, right) ->
       "(" ^ to_string left ^ " + " ^ to_string right ^ ")"
    | Minus (left, right) ->
       "(" ^ to_string left ^ " - " ^ to_string right ^ ")"
    | Times (left, right) ->
       "(" ^ to_string left ^ " * " ^ to_string right ^ ")"
    | Divide (left, right) ->
       "(" ^ to_string left ^ " / " ^ to_string right ^ ")"
