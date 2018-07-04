module Kit.Compiler.TypeUsage where

  import Data.IORef
  import Kit.Ast
  import Kit.HashTable
  import Kit.Str

  data TypeUsage = TypeUsage {
    type_definition :: TypeDefinition,
    monomorphs :: HashTable Str [TypeSpec]
  } deriving (Show)

  newTypeUsage :: TypeDefinition -> IO TypeUsage
  newTypeUsage typeDefinition = do
    monomorphs <- h_new
    return $ TypeUsage {
      type_definition = typeDefinition,
      monomorphs = monomorphs
    }

  sMonomorph :: [TypeSpec] -> Str
  -- TODO
  sMonomorph (h:t) = "abc"
  sMonomorph [] = ""

  recordUsage :: TypeUsage -> [TypeSpec] -> IO ()
  recordUsage t params = do
    h_insert (monomorphs t) (sMonomorph params) params
