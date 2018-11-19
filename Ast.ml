type op = Add | Sub | Mult | MMult | Div | MDiv | Equal | Neq | Less | Leq | Greater | Geq | And | Not | Or | Mod | Inc | Dec 
type primitive = Int_t | Float_t | Bool_t | Matrix_t 
type datatype =  Datatype of primitive | Any

(*type extends = NoParent | Parent of string*)
type fname = FName of string
(*type fname = Constructor | FName of string*)
type formal = Formal of primitive * string
(*type formal = Formal of datatype * string | Many of datatype*)

type expr =
		  Int_Lit of int
	| 	Boolean_Lit of bool
	| 	Float_Lit of float
	| 	Mat_Lit of expr list
	| 	Id of string
	| 	Binop of expr * op * expr
	| 	Assign of expr * expr
	| 	Noexpr
	| 	Call of string * expr list  
	|  	Unop of op * expr

type stmt =
		Block of stmt list
	| 	Expr of expr
	| 	Return of expr
	| 	If of expr * stmt * stmt
	| 	For of expr * expr * expr * stmt
	| 	While of expr * stmt
	|  	Break
	|   Continue
  |   Local of primitive * string * expr
  |   MatrixDecl of primitive * string * int * int * expr

type field = Field of (*scope **) datatype * string
type include_stmt = Include of string


type fdecl = {
	fname : string;
	returnType : primitive;
	(*formals : formal list;*)
	formals : (primitive * string) list;
	body : stmt list;
}

type top_stmt =
    Function of fdecl
  | Statement of stmt 

type program = Program of include_stmt list * top_stmt list
