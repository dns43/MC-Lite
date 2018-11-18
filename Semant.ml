
open Ast
open Sast

let check_program = function
  Program(includes, top_stmts) ->

  (*let rec check_dups = function*)
    (*[] -> ()*)
    (*| ((_,n1,_) :: (_,n2,_) :: _) when n1 = n2 ->*)
        (*raise (Failure ("Found duplicate name: " ^ n1))*)
    (*| (_,n1,_) :: (_,n2,_,_,_) :: _) when n1 = n2 ->*)
        (*raise (Failure ("Found duplicate name: " ^ n1))*)
    (*| (_,n1,_,_,_) :: (_,n2,_) :: _) when n1 = n2 ->*)
        (*raise (Failure ("Found duplicate name: " ^ n1))*)
    (*| _ :: l -> check_dups l*)
  (*in*)

  (*check_dups (List.sort (fun (_,a) (_,b) -> compare a b) top_stmts);*)
    

    let check_stmt = function
        (*Block b -> SBlock(List.map check_stmt b)*)
      Continue -> SContinue
        | _ -> SBreak
      (*| Expr e -> SExpr (expr e)*)
      (*| If(p, b1, b2) -> SIf(check_bool_expr p, check_stmt b1, check_stmt b2)*)
      (*| For(e1, e2, e3, st) ->*)
		(*SFor(expr e1, check_bool_expr e2, expr e3, check_stmt st)*)
      (*| While(p, s) -> SWhile(check_bool_expr p, check_stmt s)*)
      (*| Return e -> let (t, e') = expr e in*)
        (*if t = func.typ then SReturn (t, e') *)
        (*else raise ( *)
		(*Failure ("return gives " ^ string_of_typ t ^ " expected " ^*)
			 (*string_of_typ func.typ ^ " in " ^ string_of_expr e))*)
      in
	    let check_fdecl f =
      {
        sfname = f.fname;
        sreturnType = f.returnType;
        sformals = f.formals;
        sbody = List.map check_stmt f.body;
      }

  in
  let check_top_stmt = function
    Statement stmt -> SStatement(check_stmt stmt)
    | Function f -> SFunction(check_fdecl f)


  in SProgram(includes, List.map check_top_stmt top_stmts)
