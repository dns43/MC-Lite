(* Program -> includes top_stmts
top_stmts -> top_stmt top_stmts
top_stmt -> stmt
stmt -> expr
expr -> Id
Id -> string *)

(*
LR Parse Algorithm
1.  Initialize the stack with the start state.
2.  Read an input symbol
3.  While
true do
  3.1
  if  F( top(stack), input ) = S
  then
    3.1.1
    NS 
    G( top(stack), input )
    3.1.2
    push NS
    3.1.3
    read input
    3.2
  else if  F(top(stack), input) = Rk
  then
    3.2.1
    output k
    3.2.2
    pop 
    RHS
    of production k from the stack
    NS 
    G(top(s
  tack), LHS
    k
    )
    push NS
  else if  F(top(stack), input) = A
    then
      output valid sentence
      return
    else
      output invalid sentence
      return
      *)

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

  let acc = ref false in
  let input = ref (List.hd token_list) in
  while (not !acc) do
    (* shift reduce processing here *)

  done;


	let init_program = Ast.Program([], []) in 
    init_program




