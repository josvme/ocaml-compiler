
open Ast

type action = Ast | LLVM_IR | Compile | Token

let _ =
  let action = ref Compile in
  let set_action a () = action := a in
  let speclist = [
    ("-a", Arg.Unit (set_action Ast), "Print the SAST");
    ("-l", Arg.Unit (set_action LLVM_IR), "Print the generated LLVM IR");
    ("-t", Arg.Unit (set_action Token), "Print the Tokens");
    ("-c", Arg.Unit (set_action Compile),
      "Check and print the generated LLVM IR (default)");
  ] in  
  let usage_msg = "usage: ./microc.native [-a|-l|-c] [file.mc]" in
  let channel = ref stdin in
  Arg.parse speclist (fun filename -> channel := open_in filename) usage_msg;
  let lexbuf = Lexing.from_channel !channel in
(*  Utils.token_to_strings lexbuf; *)

  try
  let ast = Parser.program Scanner.token lexbuf in
  match !action with
    Ast -> print_endline (Ast.string_of_program ast)
    | LLVM_IR -> print_string (Llvm.string_of_llmodule (Codegen.translate ast))
    | Token -> Utils.token_to_strings lexbuf
    | _ -> print_endline "Error"
  with s -> raise s