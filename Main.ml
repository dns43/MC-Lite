(*
We use Lexer and Parser,
two common OCAML Modules
*)
open Exceptions
open Filepath

(* __init__ *)
let () =	
(* verify user input *)
    try
        let filename = 
            if Array.length Sys.argv = 1 then
                print_string "Help"
	  	    else raise (Exceptions.InvalidNumberCompilerArguments (Array.length Sys.argv))
        in 

    (*execute compilation step by step*)
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
    	
        (* open file *)
    	let file_in 	= open_in filename in
    
        let lexbuf = Lexing.from_channel (file_in ()) in
		
        (* split in to tokens *)
	    let token_list 	= Processor.build_token_list (lexbuf ()) in
        (*print_string (Utils.token_list_to_string (token_list ()))*)
        
        (* build parse tree *)
        let program	= Processor.parser filename (token_list ()) in
        program
	    (*print_string (pretty_to_string  (Utils.print_tree (program ())))*)
