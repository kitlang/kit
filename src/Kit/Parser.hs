module Kit.Parser(
  module Kit.Parser.Base,
  module Kit.Parser.Lexer,
  module Kit.Parser.Parser,
  module Kit.Ast.Span,
  module Kit.Parser.Token,
  parseString,
  parseFile
) where

import Kit.Ast
import Kit.Parser.Base
import Kit.Parser.Lexer
import Kit.Parser.Parser
import Kit.Ast.Span
import Kit.Parser.Token
import Kit.Str

parseString :: Str -> Parser [Statement]
parseString s = parseTokens (scanTokens "" s)

parseFile :: FilePath -> IO (Parser [Statement])
parseFile f = do
  contents <- s_readFile f
  return $ parseTokens (scanTokens f contents)
