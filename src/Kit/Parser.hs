module Kit.Parser(
  module Kit.Parser.Base,
  module Kit.Parser.Lexer,
  module Kit.Parser.Parser,
  module Kit.Parser.Span,
  module Kit.Parser.Token,
  parseString,
  parseFile
) where

  import Kit.Ast
  import Kit.Error
  import Kit.Parser.Base
  import Kit.Parser.Lexer
  import Kit.Parser.Parser
  import Kit.Parser.Span
  import Kit.Parser.Token
  import Kit.Str

  parseString :: Str -> Parser [Expr]
  parseString s = parseTokens (scanTokens s)

  parseFile :: FilePath -> IO (Parser [Expr])
  parseFile f = do
    contents <- s_readFile f
    return $ case parseString contents of
      ParseResult r -> ParseResult r
      Err e -> Err $ errf e f
