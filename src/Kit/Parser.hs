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

  parseString :: Str -> Parser [Statement]
  parseString s = parseTokens (scanTokens Nothing s)

  parseFile :: FilePath -> IO (Parser [Statement])
  parseFile f = do
    contents <- s_readFile f
    return $ parseTokens (scanTokens (Just $ s_pack f) contents)
