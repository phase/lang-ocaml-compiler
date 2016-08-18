open Ast

exception Codegen_error of string

let codegen_error s = raise (Codegen_error s)

let generate ast =
  match ast with
  | Program (imports, external_declarations) ->
    List.iter (fun x -> print_string ("#include \"" ^ x ^ ".h\"\n")) imports
