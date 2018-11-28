module L = Llvm
open Ast
open Sast

module StringMap = Map.Make(String)


let translate = function
  SProgram(includes, stop_stmts) ->
  let context = L.global_context() in
  let mc_module = L.create_module context "MC-Lite" in
  
  let i64 = L.i64_type context
  and f64 = L.double_type context
  in

  let type_to_ll = function
      Int_t -> i64
    | Float_t -> f64
    | _ -> i64
    (* need bool, also mat? *)
  in

  let main_args = Array.make 0 i64 in
  let main_type = L.function_type (L.void_type context) main_args in

  let main = L.define_function "main" main_type mc_module
  in

  let main_llbuilder = L.builder_at_end context (L.entry_block main)
  in

  let add_var (m, b) (typ, name) = 
    let var = L.build_alloca (type_to_ll typ) name b in
    (StringMap.add name var m, b)
  in
    

  let build_expr (m, b) e = match e with
    _ -> (m, b)
  in


  let build_stmt (m, b) stmt = match stmt with
      SExpr(e) -> build_expr (m, b) e
    | SLocal(typ, name, e) -> add_var (m, b) (typ, name)
    | _ -> (m, b)
  in

  let build_top_stmt (m, b) t_stmt = match t_stmt with
      SFunction(f_data) -> (m, b)
    | SStatement(stmt_data) -> build_stmt (m, b) stmt_data
  in

  List.fold_left build_top_stmt (StringMap.empty, main_llbuilder) stop_stmts;
  mc_module

  