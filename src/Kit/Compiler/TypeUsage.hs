module Kit.Compiler.TypeUsage where

  import Data.IORef
  import Kit.Ast
  import Kit.HashTable

  data TypeUsage = TypeUsage {
    type_definition :: TypeDefinition,
    monomorphs :: HashTable [TypeSpec] ()
  } deriving (Show)

  newTypeUsage :: TypeDefinition -> IO TypeUsage
  newTypeUsage typeDefinition = do
    monomorphs <- h_new
    return $ TypeUsage {
      type_definition = typeDefinition,
      monomorphs = monomorphs
    }

  recordUsage :: TypeUsage -> [TypeSpec] -> IO ()
  recordUsage t params = do
    h_insert (monomorphs t) params ()
