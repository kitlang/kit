module Kit.Ast.Base where

  import Data.List
  import System.FilePath
  import Kit.Str

  type ModulePath = [Str]

  -- (Optional module path or empty, type name)
  type TypePath = (ModulePath, Str)

  parseModulePath :: Str -> ModulePath
  parseModulePath s = s_split '.' s

  showModulePath :: ModulePath -> Str
  showModulePath s = s_join "." s

  moduleFilePath :: ModulePath -> FilePath
  moduleFilePath mod = replaceExtension (joinPath (map s_unpack mod)) ".kit"

  data Lvalue
    = Var Str
    | MacroVar Str
    deriving (Eq, Show)

  lvalue_name x = case x of
    Var s -> s
    MacroVar s -> s
