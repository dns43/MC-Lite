(*open Exceptions*)
(*open Filepath*)
open Utils

let () =	
		let filename 	= "test.mc" in
      let file_in	= fun () -> open_in filename in
      let lexbuf  = fun () -> Lexing.from_channel (file_in ()) in
      let token_list = fun () -> Processor.build_token_list (lexbuf ()) in
        print_string( Utils.token_list_to_string (token_list()) )
