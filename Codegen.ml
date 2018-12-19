module L = Llvm
open Ast
open Sast
open Printf

module StringMap = Map.Make(String)


let translate = function
  SProgram(includes, stop_stmts) ->
  let context = L.global_context() in
  let mc_module = L.create_module context "MC-Lite" in
  
  let i64 = L.i64_type context
  and f64 = L.double_type context
  and i1_t = L.i1_type context 
  in

  let type_to_ll = function
      Int_t -> i64
    | Float_t -> f64
    | Bool_t -> i1_t
    | _ -> i64 (*raise(Failure("unrecognized type")*)
    
    (* need bool, also mat? *)
  in

  let main_args = Array.make 0 i64 in
  let main_type = L.function_type (L.void_type context) main_args in

  let main = L.define_function "main" main_type mc_module
  in

  let main_llbuilder = L.builder_at_end context (L.entry_block main)
  in

  let printf_t : L.lltype = 
      L.var_arg_function_type i64 [| L.pointer_type (L.i8_type context) |] in
  let printf_func : L.llvalue = 
      L.declare_function "printf" printf_t mc_module in
  
  let int_format_str = L.build_global_stringptr "%d\n" "fmt" main_llbuilder in

  (*print function for floats*) 
 (* let printfl_t : L.lltype = 
          L.var_arg_function_type f64 [| L.pointer_type (L.i8_type context)|] in   let printfl_func : L.llvalue = 
          L.declare_function "printfl" printfl_t mc_module in 
*)
  let float_format_str = L.build_global_stringptr "%f\n" "fmt" main_llbuilder in

  let lookup n m = try StringMap.find n m
            with Not_found -> raise (Failure ("var "^n^" not found"))
  in


  let add_var (m, b) (typ, name) = 
    let var = L.build_alloca (type_to_ll typ) name b in
    (StringMap.add name var m, b)
  in

  let is_mat_t t = match t with
        Matrix_t(_,_) -> true
      | _ -> false
  in

  let mat_size mt = match mt with
      Matrix_t(r, c) -> r*c, r, c
    | _ -> raise(Failure("Codegen: Error expected Matrix_t (internal)"))
  in

  let to_ll_float = function
      Float_t, SFloat_Lit f -> L.const_float f64 f
    | _, _ -> raise (Failure ("Codegen: Error expected float (internal)"))
  in
    
  let rec build_expr (m, b) (t, e) = match e with
        SInt_Lit i  -> L.const_int i64 i
      | SBoolean_Lit b  -> L.const_int i1_t (if b then 1 else 0)
      | SFloat_Lit l -> L.const_float f64 l
      | SMat_Lit ml -> L.const_vector (Array.of_list (List.map to_ll_float  ml))
      | SNoexpr     -> L.const_int i64 0 (* TODO hacky should fix this *)
      | SId n       -> L.build_load (lookup n m) n b
      | SMIndex ((Matrix_t(r, c), me), i, j) ->
          let me' = build_expr (m, b) (Matrix_t(r, c), me) in
          let dind = L.const_int i64 (i*c + j) in
          let el = L.build_extractelement me' dind "el" b in
          el
      | SMAssign (s, i, j, (t1, e1)) ->
          let e1' = build_expr (m, b) (t1, e1) in
          let msize, r, c = mat_size t in
          let dind = L.const_int i64 (i*c + j) in
          let dest = (lookup s m) in
          let dest' = L.build_load dest "dest" b in
          let res = L.build_insertelement dest' e1' dind "ins" b in
          ignore(L.build_store res dest b);
          dest'
      | SAssign (s, (t, e1)) -> 
          let e1' = build_expr (m, b) (t, e1) in
          ignore(L.build_store e1' (lookup s m) b); 
          e1'
      
      | SBinop(e1, op, e2) when is_mat_t t ->
        let e1' = build_expr (m, b) e1
        and e2' = build_expr (m, b) e2 in
        (match op with
          Add -> (match e1, e2 with
              (Matrix_t(r1, c1), _), (Matrix_t(r2, c2), _) ->
                L.build_fadd e1' e2' "add" b
            | (Matrix_t(r1, c1), _), (Float_t, _) ->
                let msize = r1 * c1 in
                let broadcast = L.const_vector (Array.make msize e2') in
                L.build_fadd e1' broadcast "add" b
            | (Float_t, _), (Matrix_t(r1, c1), _) ->
                let msize = r1 * c1 in
                let broadcast = L.const_vector (Array.make msize e1') in
                L.build_fadd e2' broadcast "add" b
            | _, _ -> raise(Failure("Codegen Matrix Addition Unsupported Types")))
        | Sub -> (match e1, e2 with
              (Matrix_t(r1, c1), _), (Matrix_t(r2, c2), _) ->
                L.build_fsub e1' e2' "sub" b
            | (Matrix_t(r1, c1), _), (Float_t, _) ->
                let msize = r1 * c1 in
                let broadcast = L.const_vector (Array.make msize e2') in
                L.build_fsub e1' broadcast "sub" b
            | _, _ -> raise(Failure("Codegen Matrix Subtraction Unsupported Types")))
        | Mult -> (match e1, e2 with
              (Matrix_t(r1, c1), _), (Matrix_t(r2, c2), _) ->
                let msize = r1*c1 in
                let dest = L.build_alloca (L.vector_type f64 msize) "dest_a" b in
                (* init result to 0s since used in accumantion *)
                ignore(L.build_store (L.const_vector (Array.make msize (L.const_float f64 0.0))) dest b); 
                let dest' = ref (L.build_load dest "dest" b) in
                for r = 0 to r1-1 do
                  for c = 0 to c2-1 do
                    let dind = L.const_int i64 (r*c2 + c) in
                    begin
                    for i = 0 to r1-1 do
                      let ind1 = L.const_int i64 (r*c1 + i) in
                      let ind2 = L.const_int i64 (i*c2) in
                      let el1 = L.build_extractelement e1' ind1 "el1" b in
                      let el2 = L.build_extractelement e2' ind2 "el2" b in
                      let del = L.build_extractelement !dest' dind "del" b in
                      let res = L.build_fadd del (L.build_fmul el1 el2 "tmp" b) "res" b in
                      (*let dest' = L.build_insertelement dest' res dind "ins" b in*)
                      ignore(dest' := L.build_insertelement !dest' res dind "ins" b);
                    done;
                    end
                  done;
                done;
                !dest'
            | (Matrix_t(r1, c1), _), (Float_t, _) ->
                let msize = r1 * c1 in
                let broadcast = L.const_vector (Array.make msize e2') in
                L.build_fmul e1' broadcast "mul" b
            | (Float_t, _), (Matrix_t(r1, c1), _) ->
                let msize = r1 * c1 in
                let broadcast = L.const_vector (Array.make msize e1') in
                L.build_fmul e2' broadcast "mul" b
            | _, _ -> raise(Failure("Codegen Matrix Multiplication Unsupported Types")))
        | _ -> raise(Failure("Codegen Matrix Unsupported op")))
      | SUnop(Transpose, (Matrix_t(r, c), e1)) ->
          let e1' = build_expr (m, b) (Matrix_t(r, c), e1) in
          let msize = r*c in
          let dest = L.build_alloca (L.vector_type f64 msize) "dest_a" b in
          let dest' = ref (L.build_load dest "dest" b) in
          for i = 0 to r-1 do
            for j = 0 to c-1 do
              let ind1 = L.const_int i64 (i*c + j) in
              let dind = L.const_int i64 (j*r + i) in
              let el1 = L.build_extractelement e1' ind1 "el1" b in
              ignore(dest' := L.build_insertelement !dest' el1 dind "ins" b);
            done;
          done;
          !dest'
      | SBinop((Float_t, e1), op, (Float_t, e2)) ->
          let e1' = build_expr (m, b) (Float_t, e1)
          and e2' = build_expr (m, b) (Float_t, e2) in
          (match op with 
            Add     -> L.build_fadd
          | Sub     -> L.build_fsub
          | Mult    -> L.build_fmul
          | Div     -> L.build_fdiv 
          | Equal   -> L.build_fcmp L.Fcmp.Oeq
          | Neq     -> L.build_fcmp L.Fcmp.One
          | Less    -> L.build_fcmp L.Fcmp.Olt
          | Leq     -> L.build_fcmp L.Fcmp.Ole
          | Greater -> L.build_fcmp L.Fcmp.Ogt
          | Geq     -> L.build_fcmp L.Fcmp.Oge
          | And | Or ->
              raise (Failure "internal error: semant should have rejected and/or on float")
           ) e1' e2' "tmp" b
      | SBinop((Int_t, e1), op, (Int_t, e2)) ->
        let e1' = build_expr (m, b) (Int_t, e1)
        and e2' = build_expr (m, b) (Int_t, e2) in
        (match op with
          Add     -> L.build_add
        | Sub     -> L.build_sub
        | Mult    -> L.build_mul
        | Div     -> L.build_sdiv
        | And     -> L.build_and
        | Or      -> L.build_or
        | Equal   -> L.build_icmp L.Icmp.Eq
        | Neq     -> L.build_icmp L.Icmp.Ne
        | Less    -> L.build_icmp L.Icmp.Slt
        | Leq     -> L.build_icmp L.Icmp.Sle
        | Greater -> L.build_icmp L.Icmp.Sgt
        | Geq     -> L.build_icmp L.Icmp.Sge
        ) e1' e2' "tmp" b
      | SUnop(op, e) -> 
        let e' = build_expr (m, b) e in 
        (match op with 
          Neg   -> L.build_neg
        | Not   -> L.build_not) e' "temp" b 
      | SCall ("printi", [e]) ->
        L.build_call printf_func [| int_format_str ; (build_expr (m, b) e) |]
	        "printf" b
      | SCall ("printf", [e]) -> 
        L.build_call printf_func [| float_format_str ; (build_expr (m, b) e)|]
               "printf" b
      | SCall ("printmat", [(Matrix_t(r,c), me)]) ->
          let mat_format_str = ref "" in
          for i=0 to r-1 do
            for j=0 to c-1 do
              mat_format_str := !mat_format_str^"%f ";
            done;
            mat_format_str := !mat_format_str^"\n";
          done;
          let mat_format_strptr = L.build_global_stringptr !mat_format_str "fmt" b in
          let mat_vec = build_expr (m, b) (Matrix_t(r,c), me) in
          let arg_list = ref [mat_format_strptr;] in
          for i=0 to (r*c)-1 do
            let ind = L.const_int i64 i in
            let el = L.build_extractelement mat_vec ind "el" b in
            arg_list := !arg_list @ [el;];
          done;
          L.build_call printf_func (Array.of_list !arg_list)
               "printf" b
      | SCall ("printmat", [(_, _)]) ->
          raise(Failure("Codegen: printmat called with non matrix parameter"))
      | _ -> raise(Failure("Codegen: Invalid Expression"))
  in

  let add_mdecl (m, b) (md) =
    let msize = md.sncols * md.snrows in
    let vec = L.build_alloca (L.vector_type f64 msize) md.smname b in
    let m' = StringMap.add md.smname vec m in
    ignore(build_expr (m', b) (md.smtype, SAssign(md.smname, md.svalue)));
    (m', b)
  in

  let add_terminal (m, b) instr =
      match L.block_terminator (L.insertion_block b) with
	      Some _ -> ()
      | None -> ignore (instr b)
  in

  let rec build_stmt (m, b) stmt = match stmt with
      SExpr(t, e) -> ignore(build_expr (m, b) (t, e)); (m, b)
	  | SBlock sl -> List.fold_left build_stmt (m, b) sl
    | SIf (predicate, then_stmt, else_stmt) ->
          let bool_val = build_expr (m, b) predicate in
	        let merge_bb = L.append_block context "merge" main in
          let build_br_merge = L.build_br merge_bb in (* partial function *)

	        let then_bb = L.append_block context "then" main in
          let m1, b1 = build_stmt (m, (L.builder_at_end context then_bb)) then_stmt in
          ignore( build_br_merge b1 );
					(*add_terminal (build_stmt (m, (L.builder_at_end context then_bb)) then_stmt) build_br_merge;*)

	        let else_bb = L.append_block context "else" main in
	        add_terminal (build_stmt (m, (L.builder_at_end context else_bb)) else_stmt) build_br_merge;

	        ignore(L.build_cond_br bool_val then_bb else_bb b);
	        (m, L.builder_at_end context merge_bb)
    | SLocal(typ, name, (Void_t, SNoexpr)) ->
        let var = L.build_alloca (type_to_ll typ) name b in
        let (m, b) = (StringMap.add name var m, b) in
        (m, b)
    | SLocal(typ, name, e) ->
        let e' = build_expr (m, b) e in
        let var = L.build_alloca (type_to_ll typ) name b in
        let (m, b) = (StringMap.add name var m, b) in
        ignore(L.build_store e' var b);
        (m, b)
    | SMatrixDecl(md) -> add_mdecl (m, b) (md)
    | _ -> (m, b)
  in
    (*
    | SIf (predicate, then_stmt, else_stmt) -> 
        let bool_val = (build_expr(m, b) predicate) in 
        let merge_bb = L.append_block context
                        "merge" main in 
        let b_br_merge = L.build_br merge_bb in 
        let then_bb = L.append_block context 
                      "then" main in 
        add_terminal(L.builder_at_end, then_stmt)
        (L.build_br merge_bb);
        let else_bb = L.append_block context 

                    "else" main in 
        add_terminal 
        (stmt(L.builder_at_end context else_bb) else_stmt)
        b_br_merge; 

        ignore(L.build_cond_br bool_val then_bb else_bb builder);
        L.builder_at_end context merge_bb
  in *)
  let build_top_stmt (m, b) t_stmt = match t_stmt with
      SFunction(f_data) -> (m, b)
    | SStatement(stmt_data) -> build_stmt (m, b) stmt_data
  in

  let (final_m, final_b) = List.fold_left build_top_stmt (StringMap.empty, main_llbuilder) stop_stmts in
  L.build_ret_void final_b;
  mc_module

  
