module Kit.Ast.Value where

  import qualified Data.ByteString.Lazy.Char8 as B

  data ValueLiteral
    = BoolValue Bool
    | IntValue B.ByteString
    | FloatValue B.ByteString
    | StringValue B.ByteString
    deriving (Eq, Show)
