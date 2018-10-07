module L = Llvm
module A = Ast

module StringMap = Map.Make(String)

exception Error of string

let translate (globals, functions) = 
  let context = L.global_context() in 
  let the_module = L.create_module context "luttu" in 
  let i32_t  = L.i32_type  context
  and i8_t   = L.i8_type   context
  and i1_t   = L.i1_type   context
  and void_t = L.void_type context
  and str_t = L.pointer_type (L.i8_type context)
  in
  let ltype_of_typ = function
      A.Int -> i32_t
    | A.Bool -> i1_t
    | A.Unit -> void_t
    | A.Str -> str_t
  in 

  (* We define all function signatures, so that we can look them up later *)
  let function_decls =
    let function_decl m fdecl = 
      let name = fdecl.A.fname
      and formal_types = 
        Array.of_list (List.map (fun (t, _) -> ltype_of_typ t) fdecl.A.formals)
      in let ftype = L.function_type (ltype_of_typ fdecl.A.ftype) formal_types in 
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in 
    List.fold_left function_decl StringMap.empty functions in

  (* Now we define all global variables and functions 
     The structure is module, (type, number) *)
  let global_vars = 
    let global_var m (t, n) = 
      let init = L.const_int (ltype_of_typ t) 0
      in StringMap.add n (L.define_global n init the_module) m in 
    List.fold_left global_var StringMap.empty globals in 

  (* Define printf which can print things. This will be implemented in C *)
  let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in 
  let printf_func = L.declare_function "printf" printf_t the_module in 


  (* Now we build up a real function. Functions are our only basic unit of making. Expressions alone are harder 
     as we will have a hard time generating context for those things. 
     Also these functions always returns a builder *)

  let build_function fdecl = 
    let (the_function, _) = StringMap.find fdecl.A.fname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in
    let local_vars = StringMap.empty in 
    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in 
    let lookup n = try StringMap.find n local_vars with 
        Not_found -> StringMap.find n global_vars in 

    (* Generates the code for expressions 
       build_* functions normally return an llvalue. It defines llvm values and can be functions, variables, branches and more *)
    let rec codegen_expr builder = function 
      | A.Literal n -> L.const_int i32_t n
      | A.Var(t, n) -> let local = L.build_alloca (ltype_of_typ t) n builder
        in StringMap.add n local local_vars; local 
      | A.BoolLit b -> L.const_int i1_t (if b then 1 else 0)
      | A.Str s -> L.build_global_stringptr s ".str" builder
        | A.Noexpr -> L.const_int i32_t 0
      | A.Id i -> L.build_load (lookup i) i builder
      | A.Binop(e1, op, e2) -> 
        let e1l = codegen_expr builder e1
        and e2l = codegen_expr builder e2 in 
        (match op with 
           A.Add     -> L.build_add
         | A.Sub     -> L.build_sub
         | A.Mul    -> L.build_mul
         | A.Div     -> L.build_sdiv
         | A.And     -> L.build_and
         | A.Or      -> L.build_or
         | A.Equal   -> L.build_icmp L.Icmp.Eq
         | A.Neq     -> L.build_icmp L.Icmp.Ne
         | A.Less    -> L.build_icmp L.Icmp.Slt
         | A.Leq     -> L.build_icmp L.Icmp.Sle
         | A.Greater -> L.build_icmp L.Icmp.Sgt
         | A.Geq     -> L.build_icmp L.Icmp.Sge
        ) e1l e2l "tmp" builder
      | A.Unop (op, e) ->  let e' = codegen_expr builder e in 
        (match op with 
           A.Neg -> L.build_neg
         | A.Not -> L.build_not
        ) e' "tmp" builder
      | A.Assign(s, e) -> let e' = codegen_expr builder e 
        in L.build_store e' (lookup s) builder
      | A.If(cond, ipred, epred) -> 
        let bool_val = codegen_expr builder cond in 
        let merge_bb = L.append_block context "merge" the_function in 
        let then_bb = L.append_block context "then" the_function in 
        let builder_then_bb = L.builder_at_end context then_bb in 
        ignore (List.map (codegen_expr builder_then_bb) ipred);
        L.build_br merge_bb builder;
        let else_bb = L.append_block context "else" the_function in 
        let builder_else_bb = L.builder_at_end context then_bb in 
        ignore (List.map (codegen_expr builder_else_bb) epred); L.build_br merge_bb builder;
        L.build_cond_br bool_val then_bb else_bb builder
      | A.Call ("print", [e]) | A.Call ("printb", [e]) -> 
        L.build_call printf_func [| int_format_str; (codegen_expr builder e) |] "printf" builder 
      | A.Call (f, act) -> 
        let (fdef, fdecl) = StringMap.find f function_decls in 
        let actuals = List.rev (List.map (codegen_expr builder) (List.rev act)) in 
        let result = (match fdecl.A.ftype with 
              A.Unit -> ""
            | _ -> f ^ "_result") in 
        L.build_call fdef (Array.of_list actuals) result builder 
       | _ ->  L.const_int i32_t 7 
    in 
    List.map (codegen_expr builder) fdecl.body 
  in 
  List.map build_function functions; 
  the_module

let rec convert_exps exprs varss funs = 
  let (va, fnn) = match exprs with 
      [] -> (varss, funs) 
    | exp :: tl -> 
      match exp with 
        A.Func a -> convert_exps tl varss (a::funs)
      | A.Var (t, s) -> convert_exps tl ((t, s)::varss) funs
      | _ -> convert_exps tl varss funs
  in (va, fnn)

let convert_all exprs = 
  translate (convert_exps exprs [] [])