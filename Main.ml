(*open Exceptions*)
(*open Filepath*)
open Utils
open Yojson
open Parser
open SLR

let () =	
	try
		let filename 	= Sys.argv.(1) in
        let file_in	= fun () -> open_in filename in
        let lexbuf  = fun () -> Lexing.from_channel (file_in ()) in
        let token_list = fun () -> Processor.build_token_list (lexbuf ()) in
        let program = fun () -> SLR.run_slr( token_list ()) in
        (*let program = fun () -> Processor.parser filename (token_list ()) in*)
        (* let program = fun () -> SLR token_list () in *)
        (* let sprogram = fun () -> Semant.check_program (program()) in *)
        (* print_string( "filename: " ^ filename ^ "\n" ); *)
        (* print_string( Utils.token_list_to_string (token_list()) ); *)
        print_endline (pretty_to_string(Utils.print_tree (program()) ) ); 
        (*Utils.check_sprogram (sprogram());*)
        (* print_string(Llvm.string_of_llmodule (Codegen.translate (sprogram()) )); *)
        (* Llvm.print_module "mc_program.ll" (Codegen.translate (sprogram()) ); *)
        

  with
	 	Parsing.Parse_error ->
			print_string
			(
				"File \"" ^ !Processor.filename ^ "\", " ^
				"line " ^ string_of_int !Processor.line_number ^ ", " ^
				"character " ^ string_of_int !Processor.char_num ^ ", " ^
				"Syntax Error, token " ^ Utils.string_of_token_no_id !Processor.last_token ^ "\n" 
			)


