{
  open Printf
}

%token <int> NUM
%token <string> IDENTIFIER
%token EOF
%token IMPORT
%token ELLIPSIS RIGHT_ASSIGN LEFT_ASSIGN ADD_ASSIGN SUB_ASSIGN MUL_ASSIGN
%token DIV_ASSIGN MOD_ASSIGN AND_ASSIGN XOR_ASSIGN OR_ASSIGN
%token RIGHT_OP LEFT_OP INC_OP DEC_OP AND_OP OR_OP LE_OP GE_OP EQ_OP NE_OP
%token LEFT_PAREN RIGHT_PAREN RIGHT_BRACE COMMA PLUS MINUS STAR FORWARD_SLASH
%token BACK_SLASH LESS_THAN GREATER_THAN EQUAL

%start program

%%

expression
  :

variable_declaration
  : IDENTIFIER IDENTIFIER EQUAL expression

block
  : variable_declaration

/* ident = (int a, int b) { } */
function_definition
  : IDENTIFIER EQUAL LEFT_PAREN argument_list RIGHT_PAREN block

external_declaration
  : function_definition
  | variable_declaration

import
  : IMPORT IDENTIFIER

program
  : import
  | import program
  | external_declaration
  | external_declaration program
