let compile s =
  let ast = Parser.program Lexer.tokenize s in
  ast
