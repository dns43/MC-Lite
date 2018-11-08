{
	open Parser 
	let lineno = ref 1
	let depth = ref 0
	let filename = ref ""
(*
Looks overly complicated, it's just a formula to scan for STRINGS
Do we want to support strings?
We said no!
*)
	let unescape s =
		Scanf.sscanf ("\"" ^ s ^ "\"") "%S%!" (fun x -> x)
}

(*
Translation Rule Definition
*)
let alpha = ['a'-'z' 'A'-'Z']
let escape = '\\' ['\\' ''' '"' 'n' 'r' 't']
let escape_char = ''' (escape) '''
let ascii = ([' '-'!' '#'-'[' ']'-'~'])
let digit = ['0'-'9']
let id = alpha (alpha | digit | '_')*
let string = '"' ( (ascii | escape)*) '"' 
let char = ''' ( ascii | digit ) '''
let float = (digit+) ['.'] digit+
let int = digit+
let matrix = ['['] float ([','] float)* [']']
let whitespace = [' ' '\t' '\r']
let return = '\n'

rule token = parse
whitespace { token lexbuf }
| return   { incr lineno; token lexbuf}
| "//"       { incr depth; comment lexbuf }

| '('      { LPAREN }
| ')'      { RPAREN }
| '['      { LBRACKET }
| ']'      { RBRACKET }
| '{'      { LBRACE }
| '}'      { RBRACE }
| ';'      { SEMI }
| ','      { COMMA }

(* Operators *)
| ".*"     { MTIMES }
| "./"     { MDIVIDE }
| ".~"     { TRANSPOSE }
| '+'      { PLUS }
| "++"      { PLUSPLUS }
| '-'      { MINUS }
| "--"      { MINUSMINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '='      { ASSIGN }

(* Boolean Operators *)
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| "&&"     { AND }
| "||"     { OR }
| "!"      { NOT }

(* Branch Control *)
| "if"     { IF }
| "else"   { ELSE }
| "while"  { WHILE }
| "return" { RETURN }

(* Data Types *)
| "int"    { INT }
| "float"  { FLOAT }
| "bool"   { BOOL }
| "char"   { CHAR }
| "true"   { TRUE }
| "false"  { FALSE }
| "mat"    { MATRIX }
| id as lxm   { ID(lxm) }


(*names the input e.g. "5" as INT_LITERAL*)

| int as lxm          { INT_LITERAL(int_of_string lxm) }
| float as lxm        { FLOAT_LITERAL(float_of_string lxm) }
(*| matrix as lxm       { MATRIX_LITERAL(int_of_string lxm) }*)
(*| char as lxm         { CHAR_LITERAL( String.get lxm 1 ) }
| escape_char as lxm{ CHAR_LITERAL( String.get (unescape lxm) 1) }
| string              { STRING_LITERAL(unescape s) }*)
| eof                 { EOF }

(*| '"'             { raise (Exceptions.UnmatchedQuotation(!lineno)) }
| _ as illegal  { raise (Exceptions.IllegalCharacter(!filename, illegal, !lineno)) }
*)
and comment = parse
	return  { incr lineno; token lexbuf }
	(*|   "*/"    { decr depth; if !depth > 0 then comment lexbuf else token lexbuf }*)
	(*|   "/*"    { incr depth; comment lexbuf }*)
	|   _       { comment lexbuf }
