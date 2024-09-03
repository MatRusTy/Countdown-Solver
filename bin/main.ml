open Numbersgame.NonDeterministicComputations
open Numbersgame.NumbersGame
open Lettersgame.LettersGame

(* Helper functions to print and shorten solutions *)
let rec print_str_list list = 
	match list with
	| [] -> ()
	| x :: xs -> print_endline x; print_str_list xs

let first_k list k =
	let rec first_k_tail list k cumulative =
		match list, k with
		| _, 0 -> cumulative
		| [], _ -> cumulative
		| x :: xs, i -> first_k_tail xs (i-1) (x :: cumulative)
	in
	List.rev (first_k_tail list k [])

(* NUMBERS GAME *)
let input = [100; 8; 1; 8; 2; 10]
let goal = 719

let solutions = solve input goal;;

if solutions = [] then
	print_endline "No solution exists."
else
	(print_endline "Solutions:";
	print_str_list (List.map (fun r -> r.history) (first_k solutions 50)));
	print_endline ("Number of solutions: " ^string_of_int (List.length solutions));;
	print_newline ();;

(* LETTERS GAME *)
let lg_input = ['s'; 't'; 'p'; 'b'; 'u'; 'a'; 'e'; 'r'; 's']
let solutions = resolve lg_input;;

print_str_list (first_k solutions 10);
