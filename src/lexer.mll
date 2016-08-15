{
  open Parser
}

let decimal_digit = ['0'-'9']
let octal_digit = ['0'-'7']
let octal_prefix = '0' ['o' 'O']
let hex_digit = ['a'-'f' 'A'-'F' '0'-'9']
let hex_prefix = '0' ['x' 'X']
(* Any digit that isn't zero *)
let not_zero = ['1'-'9']

let identifier = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9']*

rule token = parse
  | [' ' '\t' '\n']	{ token lexbuf }

  (* Numbers *)
  | hex_prefix hex_digit+ { NUM (int_of_string lexbuf) }
  | octal_prefix octal_digit+ { NUM (int_of_string lexbuf) }
  | digit+ { NUM (int_of_string lexbuf) }
  | digit+ "." digit* { NUM (float_of_string lexbuf) }

  | identifier { IDENTIFIER lexbuf }

  (* Keywords *)
  | "import" { IMPORT }
  | "let" { LET }

  (* Symbols *)
  | "..." { ELLIPSIS }
  | ">>=" { RIGHT_ASSIGN }
  | "<<=" { LEFT_ASSIGN }
  | "+=" { ADD_ASSIGN }
  | "-=" { SUB_ASSIGN }
  | "*=" { MUL_ASSIGN }
  | "/=" { DIV_ASSIGN }
  | "%=" { MOD_ASSIGN }
  | "&=" { AND_ASSIGN }
  | "^=" { XOR_ASSIGN }
  | "|=" { OR_ASSIGN }
  | ">>" { RIGHT_OP }
  | "<<" { LEFT_OP }
  | "++" { INC_OP }
  | "--" { DEC_OP }
  | "&&" { AND_OP }
  | "||" { OR_OP }
  | "<=" { LE_OP }
  | ">=" { GE_OP }
  | "==" { EQ_OP }
  | "!-" { NE_OP }
  | "(" { LEFT_PAREN }
  | ")" { RIGHT_PAREN }
  | "{" { LEFT_BRACE }
  | "}" { RIGHT_BRACE }
  | "," { COMMA }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { STAR }
  | "/" { FORWARD_SLASH }
  | "\\" { BACK_SLASH }
  | "<" { LESS_THAN }
  | ">" { GREATER_THAN }
  | "=" { EQUAL }

  | _		  { token lexbuf }
  | eof		{ EOF }
