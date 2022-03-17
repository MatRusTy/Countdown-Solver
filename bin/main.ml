open Numbersgame.NumbersGame
open Numbersgame.Expression
open Lettersgame.LettersGame

let input = [6; 5; 4; 2; 75; 50]
let goal = 537

let solution_opt = solve input goal;;

let lg_input = ['s'; 't'; 'p'; 'b'; 'u'; 'a'; 'e'; 'r'; 's']
let solutions = resolve lg_input 10;;

print_str_list solutions;

if Option.is_none solution_opt then
	print_endline "No solution exists"
else
	(print_endline "Solution found";
	print_endline (to_string (Option.get solution_opt)))
