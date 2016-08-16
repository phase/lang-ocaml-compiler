type literal =
  | Number of int

type expression =
  | Literal of literal
  | Add of expression * expression
  | Sub of expression * expression
  | Mul of expression * expression
  | Div of expression * expression

type variable_signature =
  | VariableSignature of string * string

type variable =
  | Variable of variable_signature * expression

type statement =
  | VariableDeclaration of variable

type argument =
  | Argument of variable_signature

type function =
  | Function of variable_signature * argument list * statement list

type external_declaration =
  | GlobalVariableDeclaration of variable
  | GlobalFunctionDefinition of function

type ast =
  | Import of string
  | Program of Import list * external_declaration list
