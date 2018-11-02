(*open Exceptions*)
(*open Filepath*)
open Utils
open Yojson
open Parser

let () =	
	try
		let filename 	= "test.mc" in
        let file_in	= fun () -> open_in filename in
        let lexbuf  = fun () -> Lexing.from_channel (file_in ()) in
        let token_list = fun () -> Processor.build_token_list (lexbuf ()) in
        (*let program = fun () -> Processor.parser filename (token_list ()) in*)
        let program = fun () -> Processor.parser filename (token_list ()) in
        print_string( Utils.token_list_to_string (token_list()) );
            print_string( pretty_to_string(Utils.print_tree (program()) ) );
  with
	 	Parsing.Parse_error ->
			print_string
			(
				"File \"" ^ !Processor.filename ^ "\", " ^
				"line " ^ string_of_int !Processor.line_number ^ ", " ^
				"character " ^ string_of_int !Processor.char_num ^ ", " ^
				"Syntax Error, token " ^ Utils.string_of_token !Processor.last_token ^ "\n" 
			)


