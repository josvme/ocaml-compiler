module L = Llvm
module A = Ast

module StringMap = Hashtbl.Make(
  struct 
  type t = string
  let equal x y = x = y
  let hash = Hashtbl.hash
  end
);;
exception Error of string

let translate (globals, functions) = 
  let context = L.global_context () in 
  let the_module = L.create_module context "Luttu"
  and i32_t =  L.i32_type context
  and i8_t   = L.i8_type   context
  and i1_t   = L.i1_type   context
  and void_t = L.void_type context in

  let ltype_of_typ = function
      A.Int -> i32_t
    | A.Bool -> i1_t
    | A.Unit -> void_t in

  let global_vars = 
    let global_var m (t, n) =
      let init = L.const_int (ltype_of_typ t) 0 in 
      StringMap.add m n (L.define_global n init the_module); m in 
    List.fold_left global_var (StringMap.create 100) globals in 

  let printf_t = L.var_arg_function_type i32_t [| i32_t |] in 
  let printf_func = L.declare_function "printf" printf_t the_module in 

  let printbig_t = L.function_type i32_t [| i32_t |] in 
  let printbig_func = L.declare_function "printbig" printbig_t the_module in 

  let function_decls = 
    let function_decl m fdecl = 
      let name = fdecl.A.fname
      and formal_types = Array.of_list
          (List.map (fun (t, _) -> ltype_of_typ t) fdecl.A.formals) 
      in let ftype = L.function_type (ltype_of_typ fdecl.A.ftype) formal_types in 
      StringMap.add m name (L.define_function name ftype the_module, fdecl); m in 
    List.fold_left function_decl (StringMap.create 100) functions in 

  let build_function_body fdecl =
    let (the_function, _) = 
      StringMap.find function_decls fdecl.A.fname in 
    let builder = L.builder_at_end context (L.entry_block the_function) in
    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in 

    let local_vars =
      let add_formal m (t, n) p = L.set_value_name n p;
        (* allocate %name = alloca %value *)
        let local = L.build_alloca (ltype_of_typ t) n builder in 
        (* make a build_store v p b = store %v, %p *)
        ignore (L.build_store p local builder);
        StringMap.add m n local; m in 

      let add_local m (t, n) = 
        let local_var = L.build_alloca (ltype_of_typ t) n builder in 
        StringMap.add m n local_var; m in 

      (* combine both to allocate on stack *)
      let formals = List.fold_left2 add_formal (StringMap.create 100) fdecl.A.formals
          (Array.to_list (L.params the_function)) in 
      List.fold_left add_local formals [] in 

    let lookup n = try StringMap.find local_vars n
      with Not_found -> StringMap.find global_vars n
    in

    let add_terminal builder f =
      match L.block_terminator (L.insertion_block builder) with 
        Some _-> ()
      | None -> ignore (f builder) 
    in 

    (* Construct code for an expression; return its value *)
    (* Code for expression *)
    let rec expr builder = function
        A.Literal i -> L.const_int i32_t i 
      | A.Vardec (t, n) ->  let local_var = L.build_alloca (ltype_of_typ t) n builder in 
        StringMap.add local_vars n local_var; local_var  (* This is completely wrong as the map in immutable and changes wont be reflected*)
      | A.BoolLit b -> L.const_int i1_t (if b then 1 else 0)
      | A.Noexpr -> L.const_int i32_t 0
      (* build_load v name b, creates %name = load %v *)
      | A.Id s -> L.build_load (lookup s) s builder
      | A.Binop (e1, op, e2) ->
        let e1' = expr builder e1
        and e2' = expr builder e2 in
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
        ) e1' e2' "tmp" builder
      | A.Unop(op, e) ->
        let e' = expr builder e in
        (match op with
           A.Neg     -> L.build_neg
         | A.Not     -> L.build_not) e' "tmp" builder
      | A.Assign (s, e) -> let e' = expr builder e in
        ignore (L.build_store e' (lookup s) builder); e'
      | A.Call ("print", [e]) | A.Call ("printb", [e]) -> 
        L.build_call printf_func [| int_format_str; (expr builder e) |] "printf" builder 
      | A.Call ("printbig", [e]) -> 
        L.build_call printbig_func [| (expr builder e)|] "printbig" builder
      | A.Call (f, act) -> 
        let (fdef, fdecl) = StringMap.find function_decls f in 
        let actuals = List.rev (List.map (expr builder) (List.rev act)) in 
        let result = (match fdecl.A.ftype with 
              A.Unit -> ""
            | _ -> f ^ "_result") in 
        L.build_call fdef (Array.of_list actuals) result builder 
      | A.If (c, tl , el) -> 
        (* both then and else branches merge at a sinle point *)
        let bool_val = expr builder c in 
        let merge_bb = L.append_block context "merge" the_function in 
        let then_bb = L.append_block context "then" the_function in 
        add_terminal (ignore (expr (L.builder_at_end context then_bb) (A.ExprList tl)); L.builder_at_end context then_bb) (L.build_br merge_bb);
        let else_bb = L.append_block context "else" the_function in 
        add_terminal (ignore (expr (L.builder_at_end context else_bb) (A.ExprList el)); L.builder_at_end context else_bb) (L.build_br merge_bb);
        L.build_cond_br bool_val then_bb else_bb builder
      | A.While(c, el) -> 
        (* build_br bb b makes br %bb at position specified by builder b*)
        let pred_bb = L.append_block context "while" the_function in 
        ignore (L.build_br pred_bb builder);
        (*append_block c name f, creates a new basic block named name at end of function 
          f in context c*)
        let body_bb = L.append_block context "while_body" the_function in 
        (* builder_at_end bb creates an instruction pointer positioned at end of block block bb*)
        add_terminal (ignore (expr (L.builder_at_end context body_bb) (A.ExprList el)); L.builder_at_end context body_bb) (L.build_br pred_bb);

        let pred_builder = L.builder_at_end context pred_bb in 
        let bool_val = expr pred_builder c in 
        let merge_bb = L.append_block context "merge" the_function in 
        L.build_cond_br bool_val body_bb merge_bb pred_builder
      | A.For(c, b) -> 
        expr builder (A.While (c, b))
      | A.Return e -> 
          let x = match fdecl.A.ftype with
            A.Unit -> L.build_ret_void builder
          | _ -> L.build_ret (expr builder e) builder 
          in x
      | A.Spawn(f) -> L.const_int i32_t 5
      | A.Send(i, f) -> L.const_int i32_t i
      | A.Receive(f) -> L.const_int i32_t 5
      | A.Func (f) -> L.const_int i32_t 7
      | A.ExprList el -> 
          let k = List.fold_left expr_build_returner builder el in 
            let g = L.block_terminator (L.insertion_block k) in 
              match g with 
              Some x -> x 
              | None -> L.const_int i32_t 0
    
     and expr_build_returner builder exp = 
      ignore (expr builder exp); builder 
    in

     (* Build the code for each statement in the function *)
    let builde = expr builder (A.ExprList fdecl.A.body) in

    (* Add a return if the last block falls off the end *)
    add_terminal builder (match fdecl.A.ftype with
          A.Unit -> L.build_ret_void
        | t -> L.build_ret (L.const_int (ltype_of_typ t) 0)) 
  in 
    List.iter build_function_body functions;
    the_module