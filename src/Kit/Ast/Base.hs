module Kit.Ast.Base where

  import Data.List
  import Kit.Str

  type ModulePath = [Str]

  parseModulePath s = s_split '.' s

  data Lvalue
    = Var Str
    | MacroVar Str
    deriving (Eq, Show)

  data IncludePath
    = SystemHeader Str
    | LocalHeader Str
    deriving (Eq, Show)
