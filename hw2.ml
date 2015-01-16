(***************************************************************************)
(***************************** CONVERT GRAMMAR *****************************)
(***************************************************************************)

(* TO DO : DOES THE FUNCTION RETURN AS MAPPING?  *)
(* TO DO : WHAT IF THE GRAMMARS ARE NOT IN ORDER *)

(* Homework 1 style 
 * (S,
 * [S, [N A; N B];
 *  A, [N A; T "a"];
 *  A, [T "a"];
 *  B, [T "b"]
 * ])
 *
 * Homework 2 style
 * (S,
 * function
 * | S ->
 *		[N A; N B]
 * | A -> 
 *		[[N A; T a]; [T a]]
 * | B -> 
 *  	[T b])
 *
 * Test case
 * let grammar_1 = (S, [S, [N A; N B]; A, [N A; T "a"];A, [T "a"];B, [T "b"]]);;
 * let grammar_2 = (S, function | S -> [[N A; N B]] | A -> [[N A; T "a"]; [T "a"]] | B -> [[T "b"]]);;
 *)

let rec grammar_transform grammar last_non_term = match grammar with
	| [] -> []
	| (non_terminal, rule)::rest_rules ->
		if last_non_term = non_terminal
		then rule::(grammar_transform rest_rules non_terminal)
		else grammar_transform rest_rules non_terminal;;

let rec convert_grammar = function
	| starting_sym,grammar -> (starting_sym,(grammar_transform grammar starting_sym));;

(************************************************************************)
(***************************** PARSE PREFIX *****************************)
(************************************************************************)

type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

let rec break_down_single (starting_sym, func) rule accept derivative frag = match rule with
	| [] -> accept derivative frag
	| [head] -> (match head with
		| (N a) -> matcher (a, func) accept derivative frag
		| (T a) -> (match frag with
			| [] -> None
			| [h_str] -> 
					if h_str = a
					then accept derivative [] 
					else None
			| h_str::t_str -> 
					if h_str = a
					then (accept derivative t_str)					
					else None))
	| head::tail -> (match head with
		| (N a) -> (match (matcher (a, func) accept derivative frag) with
			| None -> None
			| Some (derivation, suffix) -> break_down_single (starting_sym, func) tail accept derivation suffix
		)
		| (T a) -> (match frag with
			| [] -> None 
			| [h_str] -> None
			| h_str::t_str -> 
					(if h_str = a
					then break_down_single (starting_sym,func) tail accept derivative t_str
					else None)))
	

and break_down (starting_sym,func) right_hand_side accept derivative frag = match right_hand_side with
	| [] -> None
	| [head] -> break_down_single (starting_sym,func) head accept (derivative @ [starting_sym, head]) frag
	| head::tail -> (match (break_down_single (starting_sym,func) head accept (derivative @ [starting_sym, head]) frag) with
		| None -> break_down (starting_sym,func) tail accept derivative frag
		| Some (derivation, suffix) -> Some (derivation, suffix))

and matcher grammar accept derivative frag = match grammar with 
	| (starting_sym, func) -> break_down grammar (func starting_sym) accept derivative frag;;

let parse_prefix grammar accept frag = matcher grammar accept [] frag;;

(*
Reminder:
1. arguement list order matters!
2. Match to something not on the left hand side?
3. After going all the way down to the terminal, how to check that the expression is actually invalid because there are non-terminal sign unresolved?

Done:
Mutually recusive
match inside match
*)









