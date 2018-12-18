

open Ast


type sexpr = primitive * sx
and sx =
      SInt_Lit of int
	| 	SBoolean_Lit of bool
	| 	SFloat_Lit of float
	| 	SMat_Lit of sexpr list
	| 	SId of string
	| 	SBinop of sexpr * op * sexpr
	| 	SAssign of string * sexpr
	| 	SNoexpr
	| 	SCall of string * sexpr list  
	|  	SUnop of op * sexpr


type smdecl = {
	smtype : primitive;
	smname : string;
  snrows : int;
  sncols : int;
  svalue : sexpr;
}


type sstmt =
		SBlock of sstmt list
	| 	SExpr of sexpr
	| 	SReturn of sexpr
	| 	SIf of sexpr * sstmt * sstmt
	| 	SFor of sexpr * sexpr * sexpr * sstmt
	| 	SWhile of sexpr * sstmt
	|  	SBreak
	|   SContinue
  |   SLocal of primitive * string * sexpr
  |   SMatrixDecl of smdecl

type sfdecl = {
	sfname : string;
	sreturnType : primitive;
	sformals : (primitive * string) list;
	sbody : sstmt list;
}


type stop_stmt =
    SFunction of sfdecl
  | SStatement of sstmt 


type sprogram = SProgram of include_stmt list * stop_stmt list


