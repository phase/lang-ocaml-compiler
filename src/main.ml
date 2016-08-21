let compile file =
  let lexbuf = Lexing.from_channel (open_in file) in
  let ast = Parser.program Lexer.tokenize lexbuf in
  let ast = Ast.analyze ast in
  let backend = new Codegen.backend_c in
  backend#generate ast

let () =
  if Array.length Sys.argv > 1 then
    let file = Array.get Sys.argv 1 in
    compile file
