open Utils
(* open Ast *)
(* open Sast *)
open Parser
open List
(* open Processor *)
(*possible global stack*)
type action = Shift | Reduce | Goto | Accept | Null 
let null = (Null,0);;

(* let state0 = [|(Shift,1); (); ();|] in *)
(*             int        $    E                     *)
let state0 = [|(Shift,1); null; null;|];;
let state1 = [|null; (Accept,0); null;|];;


let lookup state symbol = 
	let index = match symbol with 
		  INT(_) -> 0
		| EOF(_) -> 1 in 
	match state with 
	  0 -> state0.(index) 
	| 1 -> state1.(index) 

let run_slr token_list = 
	(*let process_token prg token = match token with*) 
	(*Ilet process_token prg (token,_) = match token with *) 
	let process_token (prg, stk) (token,_) = match token with  
	    INT 	-> let table_entry = (lookup (List.hd stk) INT) in 
	(prg, stk)
	  | _ -> print_string("unk\n"); (prg, stk) in
	(* let token_list = !(token_list) in  *)
	print_string(Utils.token_list_to_string(token_list));
	(*List.fold_left process_token program([],[]) token_list*)
	(* process_token (List.hd token_list) *)
	let init_program = Ast.Program([], []) in 
	(* let init_program = Ast.Program((List.init 0 include_stmt ("")), (List.init 0 top_stmt(Noexpr))) in  *)
	let (program, stack) = 
		List.fold_left process_token (init_program, [0;]) token_list in
	program
	
	