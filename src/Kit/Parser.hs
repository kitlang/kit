module Kit.Parser where

  import Kit.Str
  import Kit.Error
  import Kit.Parser.Lexer
  import Kit.Parser.Parser

  parseString s = parseTokens (scanTokens s)

  parseFile f = do
    contents <- s_readFile f
    return $ case parseString contents of
      ParseResult r -> ParseResult r
      Err e -> Err $ errf e f
