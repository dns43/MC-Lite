(*
We use Lexer and Parser,
two common OCAML Modules
*)

(* __init__ *)
let () =	
(* verify user input *)
	let action, filename = 
		if Array.length Sys.argv = 1 then
			Help, ""
	  	else raise (Exceptions.InvalidNumberCompilerArguments (Array.length Sys.argv)) 
	in 

(*execute compilation step by step*)
	(* open file *)
	let file_in 	= fun () -> open_in filename in
	
	(*
	##########
	## OCAML has a built-in Lexelizer (is that a word?) Module called Lexing
	## Lexicalization (is that a word?) returns file content as the following struct:
	#########
	type lexbuf = {
  	refill_buff : lexbuf -> unit;
  	mutable lex_buffer : bytes;
  	mutable lex_buffer_len : int;
  	mutable lex_abs_pos : int;
  	mutable lex_start_pos : int;
  	mutable lex_curr_pos : int;
  	mutable lex_last_pos : int;
  	mutable lex_last_action : int;
  	mutable lex_eof_reached : bool;
  	mutable lex_mem : int array;
  	mutable lex_start_p : position;
  	mutable lex_curr_p : position;
	}
	*)
	let lexbuf 	= fun () -> Lexing.from_channel (file_in ()) in
	(* split in to tokens *)
	let token_list 	= fun () -> Processor.build_token_list (lexbuf ()) in
	(* build parse tree*)
	let program 	= fun () -> Processor.parser filename (token_list ()) in
