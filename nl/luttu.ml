
open Ast
open Utils

type action = Token | Ast | LLVM_IR | Compile

let _ =
  let action = ref Compile in
  let set_action a () = action := a in
  let speclist = [
    ("-t", Arg.Unit (set_action Token), "Print the Tokens");
    ("-a", Arg.Unit (set_action Ast), "Print the AST");
    ("-l", Arg.Unit (set_action LLVM_IR), "Print the generated LLVM IR");
    ("-c", Arg.Unit (set_action Compile),
     "Check and print the generated LLVM IR (default)");
  ] in  
  let usage_msg = "usage: ./luttuc.native [-a|-l|-c|-t] [file.mc]" in
  let channel = ref stdin in
  Arg.parse speclist (fun filename -> channel := open_in filename) usage_msg;
  let lexbuf = Lexing.from_channel !channel in
  try
    let ast = Parser.program Scanner.token lexbuf in
    match !action with
      Ast -> 
      print_endline (Ast.string_of_program ast)
    | Token -> Utils.token_to_strings lexbuf
    | LLVM_IR -> print_string (Llvm.string_of_llmodule (Codegen.convert_all ast)) 
    | _ -> print_endline "Error"
  with s -> raise s