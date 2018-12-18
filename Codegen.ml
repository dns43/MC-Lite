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

  let to_ll_float = function
      Float_t, SFloat_Lit f -> L.const_float f64 f
    | _, _ -> raise (Failure ("Error expected float"))
  in
    
  let rec build_expr (m, b) (t, e) = match e with
        SInt_Lit i  -> L.const_int i64 i
      | SBoolean_Lit b  -> L.const_int i1_t (if b then 1 else 0)
      | SFloat_Lit l -> L.const_float f64 l
      | SMat_Lit ml -> L.const_vector (Array.of_list (List.map to_ll_float  ml))
      | SNoexpr     -> L.const_int i64 0 (* TODO hacky should fix this *)
      | SId n       -> L.build_load (lookup n m) n b
      | SAssign (s, (t, e)) -> 
          let e' = build_expr (m, b) (t, e) in
                          ignore(L.build_store e' (lookup s m) b); 
                          e'
      (*| SBinop ((Float_t,_ ) as e1, op, e2) ->*)
          (*let e1' = build_expr b e1*)
            (*and e2' = build_expr b e2 in*)
          (*(match op with *)
            (*Add     -> L.build_fadd*)
          (*| Sub     -> L.build_fsub*)
          (*| Mult    -> L.build_fmul*)
          (*| Div     -> L.build_fdiv *)
          (*| Equal   -> L.build_fcmp L.Fcmp.Oeq*)
          (*| Neq     -> L.build_fcmp L.Fcmp.One*)
          (*| Less    -> L.build_fcmp L.Fcmp.Olt*)
          (*| Leq     -> L.build_fcmp L.Fcmp.Ole*)
          (*| Greater -> L.build_fcmp L.Fcmp.Ogt*)
          (*| Geq     -> L.build_fcmp L.Fcmp.Oge*)
          (*| And | Or ->*)
              (*raise (Failure "internal error: semant should have rejected and/or on float")*)
          (* ) e1' e2' "tmp" b*)
      | SBinop(e1, op, e2) when t = Int_t ->
        let e1' = build_expr (m, b) e1
        and e2' = build_expr (m, b) e2 in
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
      | SCall ("printfloat", [e]) -> 
        L.build_call printf_func [| float_format_str ; (build_expr (m, b) e)|]
               "printf" b
      | _ -> raise(Failure("Invalid Operation"))
  in

  let add_mdecl (m, b) (md) =
    let msize = md.sncols * md.snrows in
    let vec = L.build_alloca (L.vector_type f64 msize) md.smname b in
    let m' = StringMap.add md.smname vec m in
    ignore(build_expr (m', b) (md.smtype, SAssign(md.smname, md.svalue)));
    (m', b)
  in

  let build_stmt (m, b) stmt = match stmt with
      SExpr(t, e) -> ignore(build_expr (m, b) (t, e)); (m, b)
    | SLocal(typ, name, e) -> add_var (m, b) (typ, name)
    | SMatrixDecl(md) -> add_mdecl (m, b) (md)
    | _ -> (m, b)
  in

  let build_top_stmt (m, b) t_stmt = match t_stmt with
      SFunction(f_data) -> (m, b)
    | SStatement(stmt_data) -> build_stmt (m, b) stmt_data
  in

  let (final_m, final_b) = List.fold_left build_top_stmt (StringMap.empty, main_llbuilder) stop_stmts in
  L.build_ret_void final_b;
  mc_module

  
