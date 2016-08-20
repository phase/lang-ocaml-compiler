%{
  open Printf
  open Ast
%}

%token <int> NUM
%token <string> IDENTIFIER
%token EOF DELIMITER
%token IMPORT
%token ELLIPSIS RIGHT_ASSIGN LEFT_ASSIGN ADD_ASSIGN SUB_ASSIGN MUL_ASSIGN
%token DIV_ASSIGN MOD_ASSIGN AND_ASSIGN XOR_ASSIGN OR_ASSIGN
%token RIGHT_OP LEFT_OP INC_OP DEC_OP AND_OP OR_OP LE_OP GE_OP EQ_OP NE_OP
%token LEFT_PAREN RIGHT_PAREN LEFT_BRACE RIGHT_BRACE COMMA PLUS MINUS STAR
%token FORWARD_SLASH BACK_SLASH LESS_THAN GREATER_THAN EQUAL COLON

%start program
%type <Ast.ast> program

%%

literal:
  NUM { Number $1 }
  ;

expression:
  expression PLUS expression { Add ($1, $3) }
  | expression MINUS expression { Sub ($1, $3) }
  | expression STAR expression { Mul ($1, $3) }
  | expression FORWARD_SLASH expression { Div ($1, $3) }
  | LEFT_PAREN expression RIGHT_PAREN { $2 }
  | literal { Literal $1 }
  | IDENTIFIER { Identifier $1 }
  ;

variable_signature:
  IDENTIFIER COLON IDENTIFIER { {typ = $3; name = $1} }
  | IDENTIFIER { {typ = ""; name = $1} }
  ;

variable_declaration:
  variable_signature EQUAL expression { {vsig = $1; exp = $3} }
  ;

statement:
  DELIMITER { Blank }
  | variable_declaration { VariableDeclaration $1 }
  ;

statement_list:
  /* empty */ { [] }
  | statement statement_list { $1 :: $2 }
  ;

block:
  LEFT_BRACE statement_list RIGHT_BRACE { $2 }
  ;

argument_list:
  /* empty */ { [] }
  | argument_list COMMA argument_list { List.append $1 $3 }
  | variable_signature { $1 :: [] }
  ;

function_definition:
  IDENTIFIER LEFT_PAREN argument_list RIGHT_PAREN COLON IDENTIFIER block { {fsig = {typ = $6; name = $1}; arguments = $3; statements = $7} }
  ;

external_declaration:
  function_definition { GlobalFunctionDefinition $1 }
  | variable_declaration { GlobalVariableDeclaration $1 }
  ;

external_declaration_list:
  /* empty */{ [] }
  | external_declaration external_declaration_list { $1 :: $2 }
  ;

import:
  IMPORT IDENTIFIER { $2 }
  | IMPORT IDENTIFIER DELIMITER { $2 }
  ;

import_list:
  /* empty */ { [] }
  | import import_list { $1 :: $2 }
  ;

program:
  import_list external_declaration_list { Program ($1, $2) }
  ;
