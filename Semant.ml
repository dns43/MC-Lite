
open Ast
open Sast
open Utils
open Printf

module StringMap = Map.Make(String)

let dummy = function
  _ -> SStatement(SBreak);;
let check_program = function
  Program(includes, top_stmts) ->

    let print_symbl m t n =
      if StringMap.is_empty m then print_string "create symbol table \n";
      print_string ("Symbol Table Add: " ^ n ^ "\n");
      StringMap.add n t m
    in 
	
    let add_symbol m stmt = match stmt with
        Local (t, n, e) -> print_symbl m t n
      (*| MatrixDecl(md) -> print_symbl m Bool_t md.mname*)
      | MatrixDecl(md) -> 
          (*let mat_type = Matrix_t(md.nrows, md.ncols) in*)
          print_symbl m md.mtype md.mname
      | _ -> m
    in

    let add_global_symbol m stmt = match stmt with
        Statement (s) -> add_symbol m s 
      | _ -> m
    in


    let add_functions m stmt = match stmt with
        Function f -> StringMap.add f.fname f m
      | _ -> m
    in


    let function_builtins = StringMap.add "printi" {
        fname = "printi";
        returnType = Int_t;
        formals = [(Int_t, "x")];
        body = [];
      } StringMap.empty
    in

    let function_decls = List.fold_left add_functions function_builtins top_stmts in

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

    let check_assign lvaluet rvaluet err = match lvaluet, rvaluet with
        Matrix_t(r1, c1), Matrix_t(r2, c2) when r1*c1 = r2*c2 -> lvaluet
      | Matrix_t(r1, c1), Matrix_t(r2, c2) when r1*c1 != r2*c2 ->
          raise (Failure (err^" matrix size mismatch"))
      | rt, lt when rt = lt -> lt
      | _, _ -> raise (Failure err)
    in   

    let function_data s = 
      try StringMap.find s function_decls
      with Not_found -> raise (Failure ("unrecognized function " ^ s))
    in

    let is_mat_t t = match t with
        Matrix_t(_,_) -> true
      | _ -> false
    in
    
    let rec check_expr m e = match e with
          Int_Lit i -> (Int_t, SInt_Lit i)
      |   Boolean_Lit b -> (Bool_t, SBoolean_Lit b)
      | 	Float_Lit v -> (Float_t, SFloat_Lit v)
      | 	Mat_Lit v -> (Matrix_t(List.length v, 1), SMat_Lit (List.map (check_mat_val m) v))
      | 	Id v -> (type_of_identifier m v, SId v)
      | 	Binop(e1, op, e2) ->
          let (t1, e1') = check_expr m e1 
          and (t2, e2') = check_expr m e2 in
          let same = t1 = t2 in
          let ty = match op with
            Add | Sub | Mult | Div when same && t1 = Int_t   -> Int_t
          | Add | Sub | Mult | Div when same && t1 = Float_t -> Float_t
          | Add | Sub when same && is_mat_t t1 -> (* TODO check rows/cols match *)
              let r1, c1 = match t1 with Matrix_t(r, c) -> r, c in
              let r2, c2 = match t2 with Matrix_t(r, c) -> r, c in
              if r1 = r2 && c1 = c2 then
                Matrix_t(r1, c1)
              else
                raise(Failure("Matrix dimensions must match for add/sub"))
          | Mult when same && is_mat_t t1 -> (* TODO check rows/cols match *)
              let r1, c1 = match t1 with Matrix_t(r, c) -> r, c in
              let r2, c2 = match t2 with Matrix_t(r, c) -> r, c in
              if c1 = r2 then
                Matrix_t(r1, c2)
              else
                raise(Failure("Matrix rows1 must match cols2 for multiplication"))
          | Add | Sub | Mult | Div when is_mat_t t1 && t2 = Float_t ->
              let r1, c1 = match t1 with Matrix_t(r, c) -> r, c in
              Matrix_t(r1, c1)
          | Add | Sub | Mult when t1 = Float_t && is_mat_t t2 ->
              let r1, c1 = match t2 with Matrix_t(r, c) -> r, c in
              Matrix_t(r1, c1)
          | Equal | Neq | Less | Leq | Greater | Geq
                     when same && (t1 = Int_t || t1 = Float_t) -> Bool_t
          | And | Or when same && t1 = Bool_t -> Bool_t
          | _ -> raise (Failure ("illegal binary operator "))
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
          let err = "illegal assignment "^string_of_primitive rt^" = "^string_of_primitive lt'
          in (check_assign lt rt err, SAssign(var, (rt, e')))
      | 	Noexpr -> (Void_t, SNoexpr)
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

      |   _ -> raise( Failure("Unsupported Expression (semantic analysis)") )

    and check_mat_val m v =
      let (ty, sv) = check_expr m v in
      match sv with
        SFloat_Lit _ -> Float_t, sv
      | SInt_Lit i ->  Float_t, SFloat_Lit(float_of_int i)
      | _ -> raise (Failure ("Matrix values must be numbers"))
    in
    

    let check_bool_expr m e = 
      let (t', e') = check_expr m e
      (*and err = "expected Boolean expression in "*)
      in let err = "expected Boolean expression in " ^ Utils.string_of_expr e
        ^ " instead " ^ string_of_primitive t'
      in if t' != Bool_t then raise (Failure err) else (t', e') 
    in

    let check_mdecl m md =
      (* check if MAT_LIT or NoExpr, otherwise fail *)
      let mtype_size = function Matrix_t(r, c) -> r*c, r, c in
      let msize, nrows, ncols = mtype_size md.mtype in
      (* for mat lit check if same size *)
      (* for noexpr replace with mat lit of 0s *)
      let t, se = check_expr m md.value in
      let svalue = match se with
          SMat_Lit(ml) when (List.length ml) = msize -> SMat_Lit(ml)
        | SNoexpr -> SMat_Lit(List.init msize (fun _ -> (Float_t, SFloat_Lit(0.0))))
        | SMat_Lit(ml) when List.length ml != msize ->
               raise (Failure ("Matrix assignment sizes must match"))
        | _ -> match t with
            Matrix_t(r, c) when r*c = msize -> se
          | Matrix_t(r, c) when r*c != msize ->
              raise( Failure ("Matrix Initialization size mismatch") )
          | _ -> raise (Failure ("Invalid Matrix Initialization"))
      in
      {
        smtype = md.mtype;
        smname = md.mname;
        snrows = nrows;
        sncols = ncols;
        svalue = (t, svalue);
      }
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
      | MatrixDecl(md) -> SMatrixDecl(check_mdecl m md)

  
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
