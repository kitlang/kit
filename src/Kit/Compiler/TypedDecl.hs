module Kit.Compiler.TypedDecl where

  import Kit.Ast
  import Kit.Compiler.TypedExpr
  import Kit.Str

  data TypedDecl
    = TypedFunction {
        typedFunctionName :: Str,
        typedFunctionReturnType :: ConcreteType,
        typedFunctionArgs :: [(Str, ConcreteType)],
        typedFunctionBody :: TypedExpr,
        typedFunctionVariadic :: Bool
      }
    | TypedVar {
        typedVarName :: Str,
        typedVarType :: ConcreteType,
        typedVarDefault :: Maybe TypedExpr
      }
    | TypedStruct {
        typedStructName :: Str,
        typedStructFields :: [(Str, ConcreteType, Maybe TypedExpr)]
      }
    | TypedEnum {
        typedEnumName :: Str,
        typedEnumVariants :: [TypedEnumVariant]
      }
    deriving (Eq, Show)

  data TypedEnumVariant = TypedEnumVariant {
    typedEnumVariantName :: Str,
    typedEnumVariantArgs :: [(Str, ConcreteType)]
  } deriving (Eq, Show)
