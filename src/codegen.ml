open Ast

exception Codegen_error of string

let codegen_error s = raise (Codegen_error s)

let string_of_variable_signature v = v.typ ^ " " ^ v.name

let rec string_of_expression exp =
  match exp with
  | Literal l -> (match l with
    | Number n -> string_of_int n)
  | Identifier s -> s
  | Add (e1,e2) -> "(" ^ (string_of_expression e1) ^ " + " ^ (string_of_expression e2) ^ ")"
  | Sub (e1,e2) -> "(" ^ (string_of_expression e1) ^ " - " ^ (string_of_expression e2) ^ ")"
  | Mul (e1,e2) -> "(" ^ (string_of_expression e1) ^ " * " ^ (string_of_expression e2) ^ ")"
  | Div (e1,e2) -> "(" ^ (string_of_expression e1) ^ " / " ^ (string_of_expression e2) ^ ")"

let string_of_variable v =
  (string_of_variable_signature v.vsig) ^ " = " ^ (string_of_expression v.exp) ^ ";\n"

let string_of_arguments a =
  let args = ref "" in
  List.iter (fun x -> args := !args ^ (string_of_variable_signature x) ^ ", ") a;
  let length = String.length !args in
  String.sub !args 0 (length - 2)

let string_of_statement s =
  match s with
  | Blank -> ";"
  | VariableDeclaration v -> string_of_variable v

let generate_variable v =
  print_string (string_of_variable v)

let generate_function f =
  print_string ((string_of_variable_signature f.fsig) ^ "(" ^ (string_of_arguments f.arguments) ^ ") {\n");
  List.iter (fun x -> print_string ("    " ^ (string_of_statement x))) f.statements;
  print_string "}\n"

let generate_declaration declaration =
  match declaration with
  | GlobalVariableDeclaration v -> generate_variable v; print_string "\n"
  | GlobalFunctionDefinition f -> generate_function f; print_string "\n"

let generate_declarations declarations =
  List.iter generate_declaration declarations

let generate ast =
  match ast with
  | Program (imports, external_declarations) ->
    List.iter (fun x -> print_string ("#include \"" ^ x ^ ".h\"\n")) imports;
    print_string "\n";
    generate_declarations external_declarations
