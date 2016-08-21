type literal =
  | Number of int

type expression =
  | Literal of literal
  | Identifier of string
  | Add of expression * expression
  | Sub of expression * expression
  | Mul of expression * expression
  | Div of expression * expression

type variable_signature = { typ : string; name : string }

type variable = { vsig : variable_signature; exp : expression }

type statement =
  | Blank
  | VariableDeclaration of variable

type func = { fsig : variable_signature;
              arguments : variable_signature list;
              statements : statement list;
              scope : variable_signature list }

type external_declaration =
  | GlobalVariableDeclaration of variable
  | GlobalFunctionDefinition of func

type ast = { imports : string list; declarations : external_declaration list; scope : variable_signature list }

exception Type_error of string
let type_error s = raise (Type_error s)

let rec get_type expr scope =
  match expr with
  | Literal l -> (match l with
    | Number _ -> "int")
  | Identifier s ->
    let scope = List.filter (fun x -> x.name = s) scope in
    let tail = List.hd (List.rev scope) in
    tail.typ
  | Add (e1, e2)
  | Sub (e1, e2)
  | Mul (e1, e2)
  | Div (e1, e2) ->
    let e1_type = get_type e1 scope in
    let e2_type = get_type e2 scope in
    if e1_type = e2_type
    then e1_type
    else type_error ("Mismatch types: " ^ e1_type ^ " != " ^ e2_type); ""

let analyze_declaration ast declaration =
  match declaration with
  | GlobalVariableDeclaration v ->
    if v.vsig.typ = "" then v.vsig.typ = get_type v.exp ast.scope
  | GlobalFunctionDefinition f ->
    let scope = ast.scope in
    let scope = List.append f.arguments f.scope in
    let analyze_statement statement =
      match statement with
      | Blank
      | VariableDeclaration v ->
        if v.vsig.typ = "" then v.vsign.typ = get_type v.exp scope;
        f.scope = f.scope :: v
    in List.iter analyze_statement f.statements

let analyze ast =
  List.iter (analyze_declaration ast) ast.declarations
