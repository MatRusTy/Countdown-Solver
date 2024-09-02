open Numbersgame.NumbersGame
open Lettersgame.LettersGame

(* NUMBERS GAME *)
let input = [100; 8; 1; 8; 2; 10]
let goal = 719

let solutions = solve input goal;;

if solutions = [] then
	print_endline "No solution exists"
else
	(print_endline "Solutions:";
	print_str_list (List.map (fun r -> r.history) solutions))

(* LETTERS GAME *)
let lg_input = ['s'; 't'; 'p'; 'b'; 'u'; 'a'; 'e'; 'r'; 's']
let solutions = resolve lg_input 10;;

print_str_list solutions;
