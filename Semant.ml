
open Ast
open Sast
open Utils


module StringMap = Map.Make(String)

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

    let symbols = StringMap.empty and function_decls = StringMap.empty
    in

    let check_assign lvaluet rvaluet err =
       if lvaluet = rvaluet then lvaluet else raise (Failure err)
    in   

    let function_data s = 
      try StringMap.find s function_decls
      with Not_found -> raise (Failure ("unrecognized function " ^ s))
    in



    
    let id_type s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in



    let rec check_expr = function
          Int_Lit i -> (Int_t, SInt_Lit i)
      |   Boolean_Lit b -> (Bool_t, SBoolean_Lit b)
      | 	Float_Lit v -> (Float_t, SFloat_Lit v)
      | 	Mat_Lit v -> (Matrix_t, SMat_Lit (List.map check_expr v))
      | 	Id v -> (id_type v, SId v)
      | 	Binop(e1, op, e2) ->
          let (t1, e1') = check_expr e1 
          and (t2, e2') = check_expr e2 in
          let same = t1 = t2 in
          let ty = match op with
            Add | Sub | Mult | Div when same && t1 = Int_t   -> Int_t
          | Add | Sub | Mult | Div when same && t1 = Float_t -> Float_t
          | Equal | Neq | Less | Leq | Greater | Geq
                     when same && (t1 = Int_t || t1 = Float_t) -> Bool_t
          | And | Or when same && t1 = Bool_t -> Bool_t
          | _ -> raise ( 
        Failure ("illegal binary operator "))
          in (ty, SBinop((t1, e1'), op, (t2, e2')))
      | 	Assign(v, e) ->
          let lt = match v with
              Id v -> id_type v
            | _  -> raise (Failure("Invalid assignment in " ^
                            Utils.string_of_expr e))
          in
          let (lt', v') = check_expr v in
          let (rt, e') = check_expr e in
          let err = "illegal assignment " 
          in (check_assign lt rt err, SAssign((lt', v'), (rt, e')))
      | 	Noexpr -> (Int_t, SNoexpr)
      |   Call(fname, args) as call -> 
          let fd = function_data fname in
          let param_length = List.length fd.formals in
          if List.length args != param_length then
            raise (Failure ("expecting different num args in func call") )
          else let check_call (ft, _) e = 
            let (et, e') = check_expr e in 
            let err = "illegal argument found "
            in (check_assign ft et err, e')  
          in 
          let args' = List.map2 check_call fd.formals args
          in (fd.sreturnType, SCall(fname, args'))
      | Unop(op, e) as ex -> 
          let (t, e') = check_expr e in
          let ty = match op with
            Neg when t = Int_t || t = Float_t -> t
          | Inc when t = Int_t -> t
          | Dec when t = Int_t -> t
          | Not when t = Bool_t -> Bool_t
          | _ -> raise (Failure ("illegal unary operator " ))
          in (ty, SUnop(op, (t, e')))

      |   _ -> (Int_t, SNoexpr)
    in
    let check_bool_expr e = 
      let (t', e') = check_expr e
      (*and err = "expected Boolean expression in "*)
      in let err = "expected Boolean expression in " ^ Utils.string_of_expr e
        ^ " instead " ^ string_of_primitive t'
      in if t' != Bool_t then raise (Failure err) else (t', e') 
    in

    let rec check_stmt = function
      Block b -> SBlock(List.map check_stmt b)
      | Expr e  -> SExpr(check_expr e)
      | Return r -> SReturn(check_expr r)
      | If(c, b1, b2) -> SIf(check_bool_expr c, check_stmt b1, check_stmt b2)
      | For(e1, e2, e3, s) -> SFor(check_expr e1, check_bool_expr e2, check_expr e3,
                                   check_stmt s)
      | While(c, s) -> SWhile(check_bool_expr c, check_stmt s)
      | Break -> SBreak
      | Continue -> SContinue
      | Local(d, id, e) -> SLocal(d, id, check_expr e) (* TODO needs more type checking *)
      | MatrixDecl(p, id, r, c, e) -> SMatrixDecl(p, id, r, c, check_expr e) (* TODO *)

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
