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
              statements : statement list }

type external_declaration =
  | GlobalVariableDeclaration of variable
  | GlobalFunctionDefinition of func

type ast =
  | Program of string list * external_declaration list
