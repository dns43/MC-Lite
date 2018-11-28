
open Ast
open Sast
open Utils
open Printf

module StringMap = Map.Make(String)

let dummy = function
  _ -> SStatement(SBreak);;
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
(*
      let built_in_decls = StringMap.add "print"
        (*{ typ = Void; fname = "print"; formals = [(Int, "x")];
      locals = []; body = [] } (StringMap.singleton "printb"
      { typ = Void; fname = "printb"; formals = [(Bool, "x")];
      locals = []; body = [] })*)
      in


    let symbols = StringMap.empty and function_decls = StringMap.empty
    in
    *
    let function_decls =
        List.fold_left (fun m fd -> StringMap.add fd.fname fd m)
        built_in_decls functions
    in

    let function_decl s = try StringMap.find s function_decls
        with Not_found -> raise (Failure ("unrecognized function " ^ s))
    in
*)
    let print_symbl m t n =
	if StringMap.is_empty m then print_string "create symbol table \n";
	print_string ("Symbol Table Add: " ^ n ^ "\n");
	StringMap.add n t m
   in 
	
	

    let add_symbol m stmt = match stmt with
        Local (t, n, e) -> print_symbl m t n
        | _ -> m
    in

    let add_global_symbol m stmt = match stmt with
        Statement (s) -> add_symbol m s 
        | _ -> m
    in


    let add_functions m stmt = match stmt with
        Function f ->
          StringMap.add f.fname f m
        | _ -> m
    in

    let function_decls = List.fold_left add_functions StringMap.empty top_stmts in

    let build_symbol_table stmts m =
        List.fold_left add_symbol m stmts 
    in
 
    let build_global_symbol_table stmts m =
        List.fold_left add_global_symbol m stmts 
    in   

    let type_of_identifier m s =
        try StringMap.find s m
        with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in


    


    let check_assign lvaluet rvaluet err =
       if lvaluet = rvaluet then lvaluet else raise (Failure err)
    in   



    let function_data s = 
      try StringMap.find s function_decls
      with Not_found -> raise (Failure ("unrecognized function " ^ s))
    in

 
    
    let rec check_expr m e = match e with
          Int_Lit i -> (Int_t, SInt_Lit i)
      |   Boolean_Lit b -> (Bool_t, SBoolean_Lit b)
      | 	Float_Lit v -> (Float_t, SFloat_Lit v)
      (*| 	Mat_Lit v -> (Matrix_t, SMat_Lit (List.map check_expr m v))*)
      | 	Id v -> (type_of_identifier m v, SId v)
      | 	Binop(e1, op, e2) ->
          let (t1, e1') = check_expr m e1 
          and (t2, e2') = check_expr m e2 in
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
              Id v -> type_of_identifier m v
            | _  -> raise (Failure("Invalid assignment in " ^
                            Utils.string_of_expr e))
          in
          let var = match v with
              Id n -> n
            | _  -> raise (Failure("Invalid assignment in " ^
                            Utils.string_of_expr e))
          in
          let (lt', v') = check_expr m v in
          let (rt, e') = check_expr m e in
          let err = "illegal assignment " 
          in (check_assign lt rt err, SAssign(var, (rt, e')))
      | 	Noexpr -> (Int_t, SNoexpr)
      |   Call(fname, args) as call -> 
          let fd = function_data fname in
          let param_length = List.length fd.formals in
          if List.length args != param_length then
            raise (Failure ("expecting different num args in func call") )
          else let check_call (ft, _) e = 
            let (et, e') = check_expr m e in 
            let err = "illegal argument found "
            in (check_assign ft et err, e')  
          in 
          let args' = List.map2 check_call fd.formals args
          in (fd.returnType, SCall(fname, args'))
      | Unop(op, e) as ex -> 
          let (t, e') = check_expr m e in
          let ty = match op with
            Neg when t = Int_t || t = Float_t -> t
          | Inc when t = Int_t -> t
          | Dec when t = Int_t -> t
          | Not when t = Bool_t -> Bool_t
          | _ -> raise (Failure ("illegal unary operator " ))
          in (ty, SUnop(op, (t, e')))

      |   _ -> (Int_t, SNoexpr)
    in

    let check_bool_expr m e = 
      let (t', e') = check_expr m e
      (*and err = "expected Boolean expression in "*)
      in let err = "expected Boolean expression in " ^ Utils.string_of_expr e
        ^ " instead " ^ string_of_primitive t'
      in if t' != Bool_t then raise (Failure err) else (t', e') 
    in



    let rec check_stmt m stmt = match stmt with
      Block b -> SBlock(check_block m b)
      | Expr e  -> SExpr(check_expr m e)
      | Return r -> SReturn(check_expr m r)
      | If(c, b1, b2) -> SIf(check_bool_expr m c, check_stmt m b1, check_stmt m b2)
      | For(e1, e2, e3, s) -> SFor(check_expr m e1, check_bool_expr m e2, check_expr m e3,
                                   check_stmt m s)
      | While(c, s) -> SWhile(check_bool_expr m c, check_stmt m s)
      | Break -> SBreak
      | Continue -> SContinue
      (*| Local(d, id, e) -> let symbols = set_symbol id d symbols; SLocal(d, id)  (* TODO needs more type checking *)*)
      | Local(d, id, e) -> SLocal(d, id, check_expr m e)  (* TODO needs more type checking *)
      (*| MatrixDecl(p, id, r, c, e) -> SMatrixDecl(p, id, r, c, check_expr e) (* TODO *)*)

  
    and check_block m stmts =
        let m' = build_symbol_table stmts m in
        List.map (check_stmt m') stmts
    in

  let check_fdecl m f =
    {
      sfname = f.fname;
      sreturnType = f.returnType;
      sformals = f.formals;
      sbody = check_block m f.body;
    }
  in

    let check_top_stmt m t_stmt = match t_stmt with
        Statement stmt -> SStatement(check_stmt m stmt)
      | Function f -> SFunction(check_fdecl m f)
  in 
  
  let globals =
      build_global_symbol_table top_stmts StringMap.empty
  in 
  
  SProgram(includes, List.map (check_top_stmt globals) top_stmts)
