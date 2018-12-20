open Utils
open Parser
open List
open Str

(*             int     $   E         *)
let state0 = [|"s1"; "x"; "x";|];;
let state1 = [|"x"; "acc"; "x";|];;
let state2 = [|"s1"; "x"; "x";|];;
let state3 = [|"s1"; "x"; "x";|];;
let state4 = [|"s1"; "x"; "x";|];;
let state5 = [|"s1"; "x"; "x";|];;
let state6 = [|"s1"; "x"; "x";|];;
let state7 = [|"s1"; "x"; "x";|];;

let acc = 0;;
let rule1 = "E+T";;
let rule2 = "E-T";; 
let rule3 = "T";;
let rule4 = "INT";;


let state_row = ref state0;;
let stack = ref [["0"; ""];];;
let input = ref [""];; 
let state = ref "0";;
let symbol = ref "";;
let next_symbol = ref "int";;
let parse_tree = ref "tree";;
(*let x = String.get !symbol 1;;  
print_char(x);;*)
let tup = ref [[!state; !next_symbol]];;

let shift (x) = 
	symbol := !next_symbol;
	state := x;
	tup := [[!state; !next_symbol]];
	input := List.tl !input;
	stack := !stack @ !tup;
	next_symbol := List.hd !input;;

let reduce (x) =
	if x = 1 then pop(3, "E"); tree.buildParent;
	if x = 2 pop(3, "E");
	if x = 3 pop(1, "T");
	if x = 4 pop(1, "INT"); (*choose the rule*)

	//NEED DIFFERENT LOGIC HERE// 
	symbol := !next_symbol;
	state := x;
	tup := [[!state; !next_symbol]];
	input := List.tl !input;
	stack := !stack @ !tup;
	next_symbol := List.hd !input;; (*this is where we will build the tree*)

let goto (x) = ();; (*just jump to new state and push that state onto stack*)

let accept (_) = 
acc := true;; 

(*Shift, Reduce, and goto manipulate the variables:
         stack, input, state, next_state, and symbol
     	 in order to effectively pick the next entry of the table*)

let process_state (_) =

	let decide (_) =
		let index = match !symbol with 
		"INT" -> 2
	  | "EOF" -> 3 
	  | "PLUS" -> 0
	  | "MINUS" -> 1 
	  | "E"    -> 5 
	  | "T"    -> 6 in (*finds index of row*)
	  	if !state = "0" then state_row := state0; (*finds correct row*)
	  	if !state = "1" then state_row := state1;
	  	if !state = "2" then state_row := state2;
	  	if !state = "3" then state_row := state3;
	  	if !state = "4" then state_row := state4;
	  	if !state = "5" then state_row := state5;
	  	if !state = "6" then state_row := state6;
	  	if !state = "7" then state_row := state7;
	  	let action = String.get (state_row.(index)) 1;; (*should extract correct action*)
    if action = "sX" then shift(X)
    if action = "rX" then reduce(X)
    if action = int then goto(int)
    if action = "acc" then accept();;


let run_slr token_list = 

	while acc = false (
		process_state();
	)
	
	
