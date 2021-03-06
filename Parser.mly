%{  open Ast  %}

%token CLASS EXTENDS CONSTRUCTOR INCLUDE DOT THIS PRIVATE PUBLIC
%token MATRIX INT FLOAT BOOL CHAR VOID NULL TRUE FALSE
%token SEMI LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET COMMA
%token AND NOT OR PLUS  PLUSPLUS MINUS MINUSMINUS TIMES MTIMES MDIVIDE DIVIDE ASSIGN TRANSPOSE
%token EQ NEQ LT LEQ GT GEQ BAR
%token RETURN IF ELSE FOR WHILE BREAK CONTINUE NEW DELETE 
%token <int> INT_LITERAL
%token <float> FLOAT_LITERAL
%token <string> STRING_LITERAL
%token <string> ID
%token <char> CHAR_LITERAL
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left AND OR
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left PLUSPLUS MINUSMINUS
%left TIMES MTIMES DIVIDE MDIVIDE
$right TRANSPOSE
%right NOT
%right DELETE
%right RBRACKET
%left LBRACKET 
%right DOT

%start program
%type <Ast.program> program

%%

program:
		includes top_stmts EOF { Program($1, $2) }

/******************
	TOP_STMTS
******************/

top_stmts:
		/* nothing */ { [] }
  	| 	top_stmt_list  { List.rev $1 }

top_stmt_list:
    	top_stmt              { [$1] }
  	| 	top_stmt_list top_stmt { $2::$1 }

top_stmt:
  stmt                      { Statement($1) }
  | fdecl                   { Function($1) }



/******************
	INCLUDE
******************/

includes:
		/* nothing */ { [] }
  	| 	include_list  { List.rev $1 }

include_list:
    	include_decl              { [$1] }
  	| 	include_list include_decl { $2::$1 }

include_decl:
	INCLUDE LPAREN STRING_LITERAL RPAREN SEMI { Include($3) }


/******************
 MATRICES
******************/



mat_member_list:
  expr      { [$1]  }
  | mat_member_list COMMA expr  { $3::$1 }

mat_lit:
  LBRACKET mat_member_list RBRACKET { $2 }



/******************
 METHODS
******************/
/*
fname:
	ID { $1 }
*/

fdecl:
	primitive ID LPAREN formals_opt RPAREN LBRACE stmt_list RBRACE 
	{ 
		{
      fname = $2;
			returnType = $1;
			formals = $4;
			body = List.rev $7;
		} 
	}

/******************
 FORMALS/PARAMETERS & VARIABLES & ACTUALS
******************/

formals_opt:
		/* nothing */ { [] }
	| 	formal_list   { List.rev $1 }

formal_list:
		formal                   { [$1] }
	| 	formal_list COMMA formal { $3 :: $1 }

formal:
	primitive ID { ($1, $2) }

actuals_opt:
		/* nothing */ { [] }
	| 	actuals_list  { List.rev $1 }

actuals_list:
		expr                    { [$1] }
	| 	actuals_list COMMA expr { $3 :: $1 }


/***************
	DATATYPES
***************/
primitive:
		INT 		{ Int_t }
	| 	FLOAT		{ Float_t } 
	| 	BOOL 		{ Bool_t }

type_tag:
		primitive { $1 }

matrix_type:
  MATRIX LBRACKET INT_LITERAL COMMA INT_LITERAL RBRACKET {Matrix_t($3, $5)}


datatype:
		  type_tag    { ($1) }


brackets:
		/* nothing */ 			   { 1 }
	| 	brackets RBRACKET LBRACKET { $1 + 1 }

/******************
 EXPRESSIONS
******************/

stmt_list:
		/* nothing */  { [] }
	| stmt_list stmt { $2 :: $1 }

stmt:
		expr SEMI { Expr($1) }
	| 	RETURN expr SEMI { Return($2) }
	|	RETURN SEMI		 { Return(Noexpr) }
	| 	LBRACE stmt_list RBRACE { Block(List.rev $2) }
	| 	IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([Expr(Noexpr)])) }
	| 	IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
	| 	FOR LPAREN expr_opt SEMI expr_opt SEMI expr_opt RPAREN stmt
		 { For($3, $5, $7, $9) }
	| 	WHILE LPAREN expr RPAREN stmt 	{ While($3, $5) }
	|	BREAK SEMI					 	{ Break }
	|	CONTINUE SEMI				 	{ Continue }
	|   primitive ID SEMI 			 	{ Local($1, $2, Noexpr) }
	| 	primitive ID ASSIGN expr SEMI 	{ Local($1, $2, $4) }
  | matrix_type ID SEMI
      {
        MatrixDecl({
          mtype = $1;
          mname = $2;
          value = Noexpr;
        })
      }
  | matrix_type ID ASSIGN expr SEMI                
      {
        MatrixDecl({
          mtype = $1;
          mname = $2;
          value = $4;
        })
      }


expr_opt:
		/* nothing */ { Noexpr }
	| 	expr          { $1 }


expr:
		literals		 					{ $1 }
	| 	expr PLUS   expr 					{ Binop($1, Add,   $3) }
	| 	expr PLUSPLUS             { Unop(Inc, $1) }
	| 	expr MINUS  expr 					{ Binop($1, Sub,   $3) }
	| 	expr MINUSMINUS           { Unop(Dec, $1) }
	| 	expr TIMES  expr 					{ Binop($1, Mult,  $3) }
	| 	expr MTIMES  expr 					{ Binop($1, MMult,  $3) }
	| 	expr DIVIDE expr 					{ Binop($1, Div,   $3) }
	| 	expr MDIVIDE expr 					{ Binop($1, MDiv,   $3) }
	| 	expr TRANSPOSE 	  				{ Unop(Transpose, $1) }
	| 	expr EQ     expr 					{ Binop($1, Equal, $3) }
	| 	expr NEQ    expr 					{ Binop($1, Neq,   $3) }
	| 	expr LT     expr 					{ Binop($1, Less,  $3) }
	| 	expr LEQ    expr 					{ Binop($1, Leq,   $3) }
	| 	expr GT     expr 					{ Binop($1, Greater,  $3) }
	| 	expr GEQ    expr 					{ Binop($1, Geq,   $3) }
	| 	expr AND    expr 					{ Binop($1, And,   $3) }
	| 	NOT  expr 							{ Unop (Not,   $2) }
	| 	expr OR     expr 					{ Binop($1, Or,    $3) }
	| 	expr ASSIGN expr 					{ Assign($1, $3) }
	|   MINUS expr 							{ Unop (Neg, $2) }
	| 	ID LPAREN actuals_opt RPAREN 		{ Call($1, $3) }
	| 	LPAREN expr RPAREN 					{ $2 }
	| 	expr LBRACKET expr COMMA expr RBRACKET { MIndex($1, $3, $5) }


bracket_args:
		LBRACKET expr						 { [$2] }
	| 	bracket_args RBRACKET LBRACKET expr { $4 :: $1 }


literals:
	  INT_LITERAL      		{ Int_Lit($1) }
  | LBRACKET mat_member_list RBRACKET    { Mat_Lit(List.rev $2) }
  | FLOAT_LITERAL    		{ Float_Lit($1) }
	| TRUE			   		{ Boolean_Lit(true) }
	| FALSE			   		{ Boolean_Lit(false) }
	| ID 			   		{ Id($1) }	
