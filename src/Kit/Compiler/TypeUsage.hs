module Kit.Compiler.TypeUsage where

  import Data.IORef
  import Kit.Ast
  import Kit.Compiler.Monomorphize
  import Kit.HashTable
  import Kit.Str

  data TypeUsage = TypeUsage {
    type_definition :: TypeDefinition,
    type_monomorphs :: HashTable Str TypeDefinition
  } deriving (Show)

  newTypeUsage :: TypeDefinition -> IO TypeUsage
  newTypeUsage typeDefinition = do
    monomorphs <- h_new
    return $ TypeUsage {
      type_definition = typeDefinition,
      type_monomorphs = monomorphs
    }

  sMonomorph :: [TypeSpec] -> Str
  -- TODO
  sMonomorph (h:t) = "abc"
  sMonomorph [] = ""

  getMonomorph :: TypeUsage -> [TypeSpec] -> IO (TypeDefinition)
  getMonomorph t params = do
    let def = type_definition t
    if type_params def == []
      then return def
      else do
        let key = (sMonomorph params)
        existing <- h_lookup (type_monomorphs t) (key)
        case existing of
          Just td -> return td
          Nothing -> do
            let monomorph = monomorphizeType (zip (map typeParamToSpec $ type_params def) params) def
            h_insert (type_monomorphs t) key monomorph
            return monomorph

  -- concreteTypeFromTypeDefinition :: ModulePath -> TypeDefinition -> ConcreteType
  -- concreteTypeFromTypeDefinition mod (TypeDefinition {type_name = type_name, type_type = t, type_params = params}) =
  --   case t of
  --     Atom -> TypeAtom type_name
  --     Struct {} -> TypeStruct (mod, type_name) params'
  --     Enum {} -> TypeEnum (mod, type_name) params'
  --     Abstract {} -> TypeAbstract (mod, type_name) params'
  --     Typedef {} -> TypeTypedef (mod, type_name) params'
  --   where params' =
