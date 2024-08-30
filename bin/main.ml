open Numbersgame.NumbersGame
open Numbersgame.Expression
open Lettersgame.LettersGame

(* NUMBERS GAME *)
let input = [6; 5; 4; 2; 75; 50]
let goal = 999

let solutions = solve input goal;;

if solutions = [] then
	print_endline "No solution exists"
else
	(print_endline "Solutions:";
	print_str_list (List.map to_string solutions))

(* LETTERS GAME *)
let lg_input = ['s'; 't'; 'p'; 'b'; 'u'; 'a'; 'e'; 'r'; 's']
let solutions = resolve lg_input 10;;

print_str_list solutions;
