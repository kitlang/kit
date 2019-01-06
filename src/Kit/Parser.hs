module Kit.Parser(
  module Kit.Parser.Base,
  module Kit.Parser.Lexer,
  module Kit.Parser.Parser,
  module Kit.Ast.Span,
  module Kit.Parser.Token,
  parseString,
  parseFile
) where

import qualified Data.ByteString.Lazy.Char8 as BL
import Kit.Ast
import Kit.Parser.Base
import Kit.Parser.Lexer
import Kit.Parser.Parser
import Kit.Ast.Span
import Kit.Parser.Token
import Kit.Str

parseString :: BL.ByteString -> Parser [SyntacticStatement]
parseString s = parseTokens $ scanTokens (FileSpan "") s

parseFile :: FilePath -> IO (Parser [SyntacticStatement])
parseFile f = do
  contents <- BL.readFile f
  return $ parseTokens $ scanTokens (FileSpan f) contents
