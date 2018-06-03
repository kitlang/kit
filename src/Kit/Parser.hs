module Kit.Parser where

  import qualified Data.ByteString.Lazy.Char8 as B
  import Kit.Parser.Lexer
  import Kit.Parser.Parser

  parseString s = parseTokens (scanTokens s)

  parseFile f = do
    contents <- B.readFile f
    return $ parseString contents
