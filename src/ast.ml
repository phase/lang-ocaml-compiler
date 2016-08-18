type literal =
  | Number of int

type expression =
  | Literal of literal
  | Identifier of string
  | Add of expression * expression
  | Sub of expression * expression
  | Mul of expression * expression
  | Div of expression * expression

type variable_signature =
  | VariableSignature of string * string

type variable =
  | Variable of variable_signature * expression

type statement =
  | Blank
  | VariableDeclaration of variable

type argument =
  | Argument of variable_signature

type f =
  | Function of variable_signature * argument list * statement list

type external_declaration =
  | GlobalVariableDeclaration of variable
  | GlobalFunctionDefinition of f

type ast =
  | Program of string list * external_declaration list
