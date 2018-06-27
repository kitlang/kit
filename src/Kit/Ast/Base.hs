module Kit.Ast.Base where

  import Data.List
  import Kit.Str

  type ModulePath = [Str]

  parseModulePath :: Str -> ModulePath
  parseModulePath s = s_split '.' s

  showModulePath :: ModulePath -> Str
  showModulePath s = s_join "." s

  data Lvalue
    = Var Str
    | MacroVar Str
    deriving (Eq, Show)

  data IncludePath
    = SystemHeader Str
    | LocalHeader Str
    deriving (Eq, Show)
