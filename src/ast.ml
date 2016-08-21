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

type func = {
  fsig : variable_signature;
  arguments : variable_signature list;
  statements : statement list;
  scope : variable_signature list
}

type external_declaration =
  | GlobalVariableDeclaration of variable
  | GlobalFunctionDefinition of func

type ast = {
  imports : string list;
  declarations : external_declaration list;
  scope : variable_signature list
}

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

(*
  I ran out of tylenol trying to finish this function.

  First, we match the type of declaration.
  If it's a variable:
    Check if the type empty,
    then construct a new AST Record with the new type of the variable.
  If it's a function:
    Go through every statement and check if it's a vairable declaration,
    then construct a new AST Record with the declaration of the variable set to
    the new type.

  Each AST reconstruction is over 10 lines because "immutability is good."

  Changing a value in a Record, aka _making a new one_, has a simple syntax:

    {record with value = new_value}

  How about modifiying a List in a Record?

    {record with list = (List.map (fun x -> x + 1) record.list)}

  That's a lot of boilerplate before we get to the actual checking part, which
  includes a match statement with two paths.

    {record with list = (List.map (fun x ->
      match x with
      | TypeA a -> TypeA (a + 1)
      | TypeB b -> TypeB (b + 2)
    ) record.list)}

  That's essentially what is going on. It gets a little more complicated with
  the function statements.
*)
let analyze_declaration ast declaration =
  match declaration with
  | GlobalVariableDeclaration v ->
    if v.vsig.typ = ""
    then {ast with declarations =
      (List.map (fun x ->
        match x with
        | GlobalVariableDeclaration xv ->
          if xv = v
          then GlobalVariableDeclaration {v with vsig = {v.vsig with typ = get_type v.exp ast.scope}}
          else GlobalVariableDeclaration xv
        | GlobalFunctionDefinition f -> GlobalFunctionDefinition f
      ) ast.declarations)}
    else ast
  | GlobalFunctionDefinition f ->
    let scope = List.append (List.append ast.scope f.arguments) f.scope in
    let new_ast = ref ast in
    let analyze_statement new_ast scope statement =
      match statement with
      | Blank -> ()
      | VariableDeclaration v ->
        if v.vsig.typ = ""
        then new_ast := {ast with declarations =
          (List.map (fun x ->
            match x with
            | GlobalFunctionDefinition xf ->
              if xf = f
              then GlobalFunctionDefinition {xf with statements = List.map (fun xfs ->
                  match xfs with
                  | Blank -> Blank
                  | VariableDeclaration xfsv ->
                    if xfsv = v
                    then VariableDeclaration {xfsv with vsig = {xfsv.vsig with typ = get_type xfsv.exp scope}}
                    else VariableDeclaration xfsv
                ) xf.statements; scope = (List.append xf.scope (v.vsig :: []))}
              else GlobalFunctionDefinition xf
            | GlobalVariableDeclaration v -> GlobalVariableDeclaration v
          ) ast.declarations)}
    in List.iter (analyze_statement new_ast scope) f.statements; !new_ast

let analyze ast =
  let new_ast = ref ast in
  List.iter (fun x -> new_ast := analyze_declaration ast x) ast.declarations;
  !new_ast
