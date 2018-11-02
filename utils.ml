


let string_of_token_no_id = function
  _ -> "token"
		(*LPAREN				-> "LPAREN"	*)
	(*| 	RPAREN				-> "RPAREN"	*)
	(*| 	LBRACE				-> "LBRACE"	*)
	(*| 	RBRACE				-> "RBRACE"	*)
	(*| 	SEMI				-> "SEMI"	*)
	(*| 	COMMA				-> "COMMA"	*)
	(*| 	PLUS				-> "PLUS"	*)
	(*| 	MINUS				-> "MINUS"	*)
	(*| 	TIMES				-> "TIMES"	*)
	(*| 	DIVIDE				-> "DIVIDE"	*)
	(*| 	ASSIGN				-> "ASSIGN"	*)
	(*| 	EQ					-> "EQ"*)
	(*| 	NEQ					-> "NEQ"*)
	(*| 	LT					-> "LT"*)
	(*| 	LEQ					-> "LEQ"*)
	(*| 	GT					-> "GT"*)
	(*| 	GEQ					-> "GEQ"*)
	(*| 	AND					-> "AND"*)
	(*| 	OR					-> "OR"*)
	(*| 	NOT					-> "NOT"*)
	(*| 	DOT					-> "DOT"*)
	(*| 	LBRACKET			-> "LBRACKET"		*)
	(*| 	RBRACKET			-> "RBRACKET"		*)
	(*| 	BAR					-> "BAR"*)
	(*| 	IF					-> "IF"*)
	(*| 	ELSE				-> "ELSE"	*)
	(*| 	FOR					-> "FOR"*)
	(*| 	WHILE				-> "WHILE"	*)
	(*| 	RETURN				-> "RETURN"	*)
	(*| 	INT					-> "INT"*)
	(*| 	FLOAT				-> "FLOAT"	*)
	(*| 	BOOL				-> "BOOL"	*)
	(*| 	CHAR				-> "CHAR"	*)
	(*| 	VOID				-> "VOID"	*)
	(*| 	NULL				-> "NULL"	*)
	(*| 	TRUE				-> "TRUE"	*)
	(*| 	FALSE				-> "FALSE"	*)
	(*| 	CLASS				-> "CLASS"	*)
	(*| 	CONSTRUCTOR			-> "CONSTRUCTOR"		*)
	(*| 	PUBLIC				-> "PUBLIC"	*)
	(*| 	PRIVATE				-> "PRIVATE"	*)
	(*| 	EXTENDS				-> "EXTENDS"	*)
	(*| 	INCLUDE				-> "INCLUDE"	*)
	(*| 	THIS				-> "THIS"	*)
	(*| 	BREAK				-> "BREAK"	*)
	(*| 	CONTINUE			-> "CONTINUE"	*)
	(*|   NEW 				-> "NEW"	*)
	(*| 	INT_LITERAL(i)		-> "INT_LITERAL(" ^ string_of_int i ^ ")"*)
	(*| 	FLOAT_LITERAL(f)	-> "FLOAT_LITERAL(" ^ string_of_float f ^ ")"*)
	(*| 	CHAR_LITERAL(c)		-> "CHAR_LITERAL(" ^ Char.escaped c ^ ")"*)
	(*| 	STRING_LITERAL(s)	-> "STRING_LITERAL(" ^ s ^ ")"*)
	(*| 	ID(s)				-> "ID(" ^ s ^ ")"*)
	(*| 	DELETE 				-> "DELETE"*)
	(*| 	MODULO 				-> "MODULO"*)
	(*|  	EOF					-> "EOF"*)



let token_list_to_string token_list =
  let rec helper = function
		(token, line)::tail ->
    string_of_token_no_id token ^ " " ^ helper tail
	| 	[] -> "\n"
  in helper token_list


