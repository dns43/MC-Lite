(* Program -> includes top_stmts
top_stmts -> top_stmt top_stmts
top_stmt -> stmt
stmt -> expr
expr -> Id
Id -> string *)

module StringMap = Map.Make(String)

type action = Shft | Rdc | Acc | Gto | Nul

let states = [ 
("string0", (Shft, 6)); 
("string2", (Shft, 6)); 
("string3", (Rdc, 2)); 
("string4", (Rdc, 3)); 
("string5", (Rdc, 4)); 
("string6", (Rdc, 5)); 
("$1", (Acc, 0)); 
("$7", (Rdc, 1)); 
("$7", (Rdc, 1)); 
("top_stmts0", (Gto, 1)); 
("top_stmts2", (Gto, 7)); 
("top_stmt0", (Gto, 2)); 
("top_stmt2", (Gto, 2)); 
("stmt0", (Gto, 3)); 
("stmt2", (Gto, 3)); 
("expr0", (Gto, 4)); 
("expr2", (Gto, 4)); 
("Id0", (Gto, 5)); 
("Id2", (Gto, 5)); 
 ];;

let table = ref StringMap.empty;;
let add_state = function
    (key, value) -> 
    table := StringMap.add key value !table
in
ignore(List.map add_state states);;

let stack = ref [("0", "");];;


let run_slr token_list = 
	print_string(Utils.token_list_to_string(token_list));


	let init_program = Ast.Program([], []) in 
    init_program




