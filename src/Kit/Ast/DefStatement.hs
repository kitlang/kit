module Kit.Ast.DefStatement where

import Kit.Ast.Definitions
import Kit.Ast.Modifier
import Kit.Ast.TypePath
import Kit.Ast.Types
import Kit.Ast.Span
import Kit.Error
import Kit.Str

{-
  A statement that can appear in type or trait definition scope.
-}
data DefStatement a b
  = DefField (VarDefinition a b)
  | DefMethod (FunctionDefinition a b)
  | DefRule (RewriteRule a b)
  | DefVariant (EnumVariant a b)
  deriving (Eq, Show)

-- instance Positioned (DefStatement Expr TypeSpec where
--   position (DefField v) = varPos v
--   position (DefMethod f) = functionPos f
--   position (DefRule r) = rulePos r
--   position (DefVariant v) = variantPos v


addTypeExtension :: DefStatement a b -> TypeDefinition a b -> TypeDefinition a b
addTypeExtension (DefField v) t = if elem Static (varModifiers v)
  then t
    { typeStaticFields = (v
                           { varName = subPath (typeName t) $ tpName $ varName v
                           }
                         )
      : typeStaticFields t
    }
  else case typeSubtype t of
    StructUnion { structUnionFields = fields } -> t
      { typeSubtype = StructUnion
        { structUnionFields = v : fields
        , isStruct          = isStruct $ typeSubtype t
        }
      }
    _ -> throwk $ BasicError
      "Non-static fields can only be added to a struct or union"
      (Just $ varPos v)
addTypeExtension (DefMethod f') t =
  let f = f' { functionName = subPath (typeName t) $ tpName $ functionName f' }
  in  if elem Static (functionModifiers f)
        then t { typeStaticMethods = f : typeStaticMethods t }
        else t { typeMethods = f : typeMethods t }
addTypeExtension (DefRule    r) t = t { typeRules = r : typeRules t }
addTypeExtension (DefVariant v) t = case typeSubtype t of
  e@(Enum { enumVariants = variants }) -> t
    { typeSubtype = e
      { enumVariants = (v
                         { variantName   = subPath (typeName t)
                           $ tpName
                           $ variantName v
                         , variantParent = typeName t
                         }
                       )
        : variants
      }
    }
  _ -> throwk
    $ BasicError "Variants can only be added to an enum" (Just $ variantPos v)
-- addTypeExtension _ t =
--   throwk $ BasicError "Invalid type extension" (Just $ typePos t)

addTraitExtension
  :: DefStatement a b -> TraitDefinition a b -> TraitDefinition a b
addTraitExtension (DefField v') t =
  let v = v' { varName = subPath (traitName t) $ tpName $ varName v' }
  in  if elem Static (varModifiers v)
        then t { traitStaticFields = v : traitStaticFields t }
        else throwk $ BasicError "Non-static fields can't be added to a trait"
                                 (Just $ varPos v)
addTraitExtension (DefMethod f') t =
  let f =
        f' { functionName = subPath (traitName t) $ tpName $ functionName f' }
  in  if elem Static (functionModifiers f)
        then t { traitStaticMethods = f : traitStaticMethods t }
        else t { traitMethods = f : traitMethods t }
addTraitExtension (DefRule r) t = t { traitRules = r : traitRules t }
addTraitExtension _ t =
  throwk $ BasicError "Invalid trait extension" (Just $ traitPos t)
