
(* Pretty Printer *)
open Ast
open Parser
open Processor

let save file string =
	 let channel = open_out file in
	 output_string channel string;
	 close_out channel

let string_of_primitive = function 
		Int_t 						-> "int"
    |	Matrix_t 					-> "mat"
	| 	Float_t 					-> "float"
	| 	Void_t						-> "void"
	| 	Bool_t 						-> "bool"

let rec print_brackets = function
		1 -> "[]"
	| 	a -> "[]" ^ print_brackets (a - 1)
		
let string_of_datatype = function 
	 	Datatype(p)		-> (string_of_primitive p)
	|  	Any 			-> "Any"

(* Print expressions *)

let string_of_op = function
		Add			-> "+"	
	 | 	Sub			-> "-"	
	 | 	Inc			-> "++"	
	 | 	Dec			-> "--"	
	 | 	Mult		-> "*"	
	 | 	MMult		-> ".*"	
	 | 	Div			-> "/"	
	 | 	MDiv			-> "./"	
	 | 	Equal		-> "=="		
	 | 	Neq			-> "!="	
	 | 	Less		-> "<"	
	 | 	Leq			-> "<="	
	 | 	Greater		-> ">"			
	 | 	Geq			-> ">="	
	 | 	And			-> "and"	
	 | 	Not			-> "not"	
	 | 	Or			-> "or"
	 | 	Mod 		-> "%"

let rec string_of_bracket_expr = function
		[] 				-> ""
	| 	head :: tail 	-> "[" ^ (string_of_expr head) ^ "]" ^ (string_of_bracket_expr tail)
and string_of_array_primitive = function
		[] 				-> ""
	|   [last]			-> (string_of_expr last)
	| 	head :: tail 	-> (string_of_expr head) ^ ", " ^ (string_of_array_primitive tail)
and string_of_expr = function 
		Int_Lit(i)				-> string_of_int i
	|	Boolean_Lit(b)			-> if b then "true" else "false"
	|	Float_Lit(f)			-> string_of_float f
	|	String_Lit(s)			-> "\"" ^ (String.escaped s) ^ "\""
	|	Id(s)					-> s
	|	Binop(e1, o, e2)		-> (string_of_expr e1) ^ " " ^ (string_of_op o) ^ " " ^ (string_of_expr e2)
	|	Assign(e1, e2)			-> (string_of_expr e1) ^ " = " ^ (string_of_expr e2)
	|	Noexpr					-> ""
	|	Call(f, el)				-> f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
	|	Mat_Lit(el)		-> "[" ^ (string_of_array_primitive el) ^ "]"
	|  	Unop(op, e)				-> (string_of_op op) ^ "(" ^ string_of_expr e ^ ")"
	|	Null					-> "null"
;;


let string_of_local_expr = function
		Noexpr -> ""
	|  	e 	   -> " = " ^ string_of_expr e




let rec string_of_stmt indent =
	let indent_string = String.make indent '\t' in
	let get_stmt_string = function 

			Block(stmts) 			-> 
				indent_string ^ "{\n" ^ 
					String.concat "" (List.map (string_of_stmt (indent+1)) stmts) ^ 
				indent_string ^ "}\n"

		| 	Expr(expr) 				-> 
				indent_string ^ string_of_expr expr ^ ";\n";

		| 	Return(expr) 			-> 
				indent_string ^ "return " ^ string_of_expr expr ^ ";\n";

		| 	If(e, s, Block([Expr(Noexpr)])) 	-> 
				indent_string ^ "if (" ^ string_of_expr e ^ ")\n" ^ 
					(string_of_stmt (indent+1) s)

		| 	If(e, s1, s2) 			-> 
				indent_string ^ "if (" ^ string_of_expr e ^ ")\n" ^ 
					string_of_stmt (indent+1) s1 ^ 
				indent_string ^ "else\n" ^ 
					string_of_stmt (indent+1) s2

		| 	For(e1, e2, e3, s) 		-> 
				indent_string ^ "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^ string_of_expr e3  ^ ")\n" ^ 
					string_of_stmt (indent) s

		| 	While(e, s) 			-> 
				indent_string ^ "while (" ^ string_of_expr e ^ ")\n" ^ 
					string_of_stmt (indent) s

		|  	Break					-> indent_string ^ "break;\n"
		|  	Continue				-> indent_string ^ "continue;\n"
		|   Local(d, s, e) 			-> indent_string ^ string_of_datatype d ^ " " ^ s ^ string_of_local_expr e ^ ";\n"
    |   MatrixDecl(m, n, r, c, e) -> indent_string ^ "mat "^n^"["^string_of_int r^","^string_of_int c^"]"^ string_of_local_expr e^";\n"
	in get_stmt_string



let string_of_fname = function 
		Constructor -> "constructor"
	|	FName(s)	-> s

let string_of_formal = function
		Formal(d, s) -> (string_of_datatype d) ^ " " ^ s
	|  	_ 			 -> ""

let string_of_formal_name = function
		Formal(_, s) -> s
	| 	_ -> ""

let string_of_func_decl fdecl =
	 (string_of_datatype fdecl.returnType) ^ " " ^ fdecl.fname ^ " " ^ 
	 (*(string_of_datatype fdecl.returnType) ^ " " ^ (string_of_fname fdecl.fname) ^ " " ^ *)
	(* Formals *)
	"(" ^ String.concat "," (List.map string_of_formal fdecl.formals) ^ ") {\n" ^
		String.concat "" (List.map (string_of_stmt 2) fdecl.body) ^
	"\t}\n\n"


let rec string_of_include = function 
	Include(s) -> "include(" ^ s ^ ");\n"

(* Print whole program *)

let string_of_program = function
	Program(includes, (*cdecls*)cbody) -> 
		String.concat "" (List.map string_of_include includes) ^ "\n" 

(* Print AST tree representation *)

let includes_tree includes = 
	`List (List.map (function Include s -> `String s) includes)

let map_fields_to_json fields = 
	`List (List.map (function Field((*scope,*) datatype, s) -> 
		`Assoc [
			("name", `String s);
			("datatype", `String (string_of_datatype datatype));
		]) fields)

let map_formals_to_json formals = 
	`List (List.map (function Formal(d, s) -> `Assoc [
													("name", `String s);
													("datatype", `String (string_of_datatype d));
												]
							  | Many d -> `Assoc [("Many", `String (string_of_datatype d));]
		) formals)

let rec map_expr_to_json = function 
		Int_Lit(i)				-> `Assoc [("int_lit", `Int i)]
	|	Boolean_Lit(b)			-> `Assoc [("bool_lit", `Bool b)]
	|	Float_Lit(f)			-> `Assoc [("float_lit", `Float f)]
	|	String_Lit(s)			-> `Assoc [("string_lit", `String s)]
	|	Id(s)					-> `Assoc [("id", `String s)]
	|	Binop(e1, o, e2)		-> `Assoc [("binop", `Assoc [("lhs", map_expr_to_json e1); ("op", `String (string_of_op o)); ("rhs", map_expr_to_json e2)])]
	|	Assign(e1, e2)			-> `Assoc [("assign", `Assoc [("lhs", map_expr_to_json e1); ("op", `String "="); ("rhs", map_expr_to_json e2)])]
	|	Noexpr					-> `String "noexpr"
	|	Call(f, el)				-> `Assoc [("call", `Assoc ([("name", `String f); ("params", `List (List.map map_expr_to_json el)); ]) )]
	|  	Unop(op, e)				-> `Assoc [("Unop", `Assoc [("op", `String (string_of_op op)); ("operand", map_expr_to_json e)])]
	|	Null					-> `String "null"
	|   ObjectCreate(s, el) 	-> `Assoc [("objectcreate", `Assoc [("type", `String s); ("args", `List (List.map map_expr_to_json el))])]
	| 	Delete(e) 				-> `Assoc [("delete", `Assoc [("expr", map_expr_to_json e)])]

let rec map_stmt_to_json = function
		Block(stmts) 			-> `Assoc [("block", `List (List.map (map_stmt_to_json) stmts))]
	| 	Expr(expr) 				-> `Assoc [("expr", map_expr_to_json expr)]
	| 	Return(expr) 			-> `Assoc [("return", map_expr_to_json expr)]
	| 	If(e, s1, s2) 			-> `Assoc [("if", `Assoc [("cond", map_expr_to_json e); ("ifbody", map_stmt_to_json s1)]); ("else", map_stmt_to_json s2)]
	| 	For(e1, e2, e3, s) 		-> `Assoc [("for", `Assoc [("init", map_expr_to_json e1); ("cond", map_expr_to_json e2); ("inc", map_expr_to_json e3); ("body", map_stmt_to_json s)])]
	| 	While(e, s) 			-> `Assoc [("while", `Assoc [("cond", map_expr_to_json e); ("body", map_stmt_to_json s)])]
	|  	Break					-> `String "break"
	|  	Continue				-> `String "continue"
	|   Local(d, s, e) 			-> `Assoc [("local", `Assoc [("datatype", `String (string_of_datatype d)); ("name", `String s); ("val", map_expr_to_json e)])]

let map_methods_to_json methods = 
	`List (List.map (fun (fdecl:Ast.fdecl) -> 
		`Assoc [
			("name", `String (fdecl.fname));
			(*("name", `String (string_of_fname fdecl.fname));*)
			(*("scope", `String (string_of_scope fdecl.scope));*)
			("returnType", `String (string_of_datatype fdecl.returnType));
			("formals", map_formals_to_json fdecl.formals);
			("body", `List (List.map (map_stmt_to_json) fdecl.body));
		]) methods)

let map_top_stmt_to_json = function
      Function(fdecl) -> `Assoc [("fdecl", `String (string_of_func_decl fdecl))]
  |   Statement(stmt)     -> `Assoc [("stmt", `String ((string_of_stmt 0) stmt))]



let top_statements_list top_statements = 
    `List (List.map (map_top_stmt_to_json) top_statements)

let print_tree = function
	Program(includes, top_statements) -> 
		`Assoc [("program", 
			`Assoc([
				("includes", includes_tree includes);
				("top_statements", top_statements_list top_statements);
				(*("classes", cdecls_tree cdecls)*)
			])
		)]

let string_of_token_no_id = function
		LPAREN				-> "LPAREN"	
	| 	RPAREN				-> "RPAREN"	
	| 	LBRACE				-> "LBRACE"	
	| 	RBRACE				-> "RBRACE"	
	| 	SEMI				-> "SEMI"	
	| 	COMMA				-> "COMMA"	
	| 	PLUS				-> "PLUS"	
	| 	PLUSPLUS				-> "PLUSPLUS"	
	| 	MINUS				-> "MINUS"	
	| 	MINUSMINUS				-> "MINUSMINUS"	
	| 	TIMES				-> "TIMES"	
	| 	MTIMES				-> "MTIMES"	
	| 	DIVIDE				-> "DIVIDE"	
	| 	MDIVIDE				-> "MDIVIDE"	
	| 	ASSIGN				-> "ASSIGN"	
	| 	EQ					-> "EQ"
	| 	NEQ					-> "NEQ"
	| 	LT					-> "LT"
	| 	LEQ					-> "LEQ"
	| 	GT					-> "GT"
	| 	GEQ					-> "GEQ"
	| 	AND					-> "AND"
	| 	OR					-> "OR"
	| 	NOT					-> "NOT"
	| 	DOT					-> "DOT"
	| 	LBRACKET			-> "LBRACKET"		
	| 	RBRACKET			-> "RBRACKET"		
	| 	BAR					-> "BAR"
	| 	IF					-> "IF"
	| 	ELSE				-> "ELSE"	
	| 	FOR					-> "FOR"
	| 	WHILE				-> "WHILE"	
	| 	RETURN				-> "RETURN"	
	| 	INT					-> "INT"
    | 	MATRIX				-> "MATRIX"
	| 	FLOAT				-> "FLOAT"	
	| 	BOOL				-> "BOOL"	
	| 	CHAR				-> "CHAR"	
	| 	VOID				-> "VOID"	
	| 	NULL				-> "NULL"	
	| 	TRUE				-> "TRUE"	
	| 	FALSE				-> "FALSE"	
	| 	CLASS				-> "CLASS"	
	| 	CONSTRUCTOR			-> "CONSTRUCTOR"		
	| 	PUBLIC				-> "PUBLIC"	
	| 	PRIVATE				-> "PRIVATE"	
	| 	EXTENDS				-> "EXTENDS"	
	| 	INCLUDE				-> "INCLUDE"	
	| 	THIS				-> "THIS"	
	| 	BREAK				-> "BREAK"	
	| 	CONTINUE			-> "CONTINUE"	
	|   NEW 				-> "NEW"		
	| 	INT_LITERAL(i)		-> "INT_LITERAL"
	| 	FLOAT_LITERAL(f)	-> "FLOAT_LITERAL"
	| 	CHAR_LITERAL(c)		-> "CHAR_LITERAL"
	| 	STRING_LITERAL(s)	-> "STRING_LITERAL"
	| 	ID(s)				-> "ID"
	| 	DELETE 				-> "DELETE"
	|  	EOF					-> "EOF"

let token_list_to_string token_list =
  let rec helper = function
		(token, line)::tail ->
		string_of_token_no_id token ^ " " ^ helper tail
	| 	[] -> "\n"
  in helper token_list

