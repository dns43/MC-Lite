

open Ast


type sexpr = primitive * sx
and sx =
		Int_Lit of int
	| 	Boolean_Lit of bool
	| 	Float_Lit of float
	| 	Mat_Lit of sexpr list
  | 	String_Lit of string
	| 	Id of string
	| 	Binop of sexpr * op * sexpr
	| 	Assign of sexpr * sexpr
	| 	Noexpr
  | 	ArrayCreate of datatype * sexpr list
  | 	ArrayAccess of sexpr * sexpr list
  | 	ObjAccess of sexpr * sexpr
	| 	Call of string * sexpr list  
  |   ObjectCreate of string * sexpr list
  | 	ArrayPrimitive of sexpr list
	|  	Unop of op * sexpr
	| 	Null
  | 	Delete of sexpr

type sstmt =
		Block of sstmt list
	| 	Expr of sexpr
	| 	Return of sexpr
	| 	If of sexpr * sstmt * sstmt
	| 	For of sexpr * sexpr * sexpr * sstmt
	| 	While of sexpr * sstmt
	|  	Break
	|   Continue
  |   Local of datatype * string * sexpr
  |   MatrixDecl of primitive * string * int * int * sexpr

type sfdecl = {
	sfname : string;
	returnType : datatype;
	formals : formal list;
	body : sstmt list;
}


type stop_stmt =
    Function of sfdecl
  | Statement of sstmt 


type sprogram = Program of include_stmt list * stop_stmt list


