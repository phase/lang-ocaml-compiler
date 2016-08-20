open Ast

exception Codegen_error of string

let codegen_error s = raise (Codegen_error s)

(* Interface for Codegens *)
type codegen = <
  (* AST to String functions *)
  string_of_variable_signature : variable_signature -> string;
  string_of_expression : expression -> string;
  string_of_variable : variable -> string;
  string_of_arguments : variable_signature list -> string;
  string_of_statement : statement -> string;

  (* Generate functions *)
  generate_variable : variable;
  generate_function : func;
  generate_declarations : external_declaration list;
  generate : ast;
>

class backend_c = object(self)
  method string_of_variable_signature v = v.typ ^ " " ^ v.name

  method string_of_expression exp =
    let rec sexp e =
      match e with
      | Literal l -> (match l with
        | Number n -> string_of_int n)
      | Identifier s -> s
      | Add (e1,e2) -> "(" ^ (sexp e1) ^ " + " ^ (sexp e2) ^ ")"
      | Sub (e1,e2) -> "(" ^ (sexp e1) ^ " - " ^ (sexp e2) ^ ")"
      | Mul (e1,e2) -> "(" ^ (sexp e1) ^ " * " ^ (sexp e2) ^ ")"
      | Div (e1,e2) -> "(" ^ (sexp e1) ^ " / " ^ (sexp e2) ^ ")"
    in
    sexp exp

  method string_of_variable v =
    (self#string_of_variable_signature v.vsig) ^ " = " ^ (self#string_of_expression v.exp) ^ ";\n"

  method string_of_arguments a =
    let args = ref "" in
    List.iter (fun x -> args := !args ^ (self#string_of_variable_signature x) ^ ", ") a;
    let length = String.length !args in
    String.sub !args 0 (length - 2)

  method string_of_statement s =
    match s with
    | Blank -> ";"
    | VariableDeclaration v -> self#string_of_variable v

  method generate_variable v =
    print_string (self#string_of_variable v)

  method generate_function f =
    print_string ((self#string_of_variable_signature f.fsig) ^ "(" ^ (self#string_of_arguments f.arguments) ^ ") {\n");
    List.iter (fun x -> print_string ("    " ^ (self#string_of_statement x))) f.statements;
    print_string "}\n"

  method generate_declaration declaration =
    match declaration with
    | GlobalVariableDeclaration v -> self#generate_variable v; print_string "\n"
    | GlobalFunctionDefinition f -> self#generate_function f; print_string "\n"

  method generate_declarations declarations =
    List.iter self#generate_declaration declarations

  method generate ast =
    match ast with
    | Program (imports, external_declarations) ->
      List.iter (fun x -> print_string ("#include \"" ^ x ^ ".h\"\n")) imports;
      print_string "\n";
      self#generate_declarations external_declarations
end
