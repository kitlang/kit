module Kit.Compiler.Typers.TypeType where

import Control.Monad
import Data.IORef
import Data.List
import Kit.Ast
import Kit.Compiler.Binding
import Kit.Compiler.Context
import Kit.NameMangling
import Kit.Compiler.Module
import Kit.Compiler.Scope
import Kit.Compiler.TypeContext
import Kit.Compiler.TypedDecl
import Kit.Compiler.TypedExpr
import Kit.Compiler.Typers.Base
import Kit.Compiler.Typers.TypeExpression
import Kit.Compiler.Typers.TypeFunction
import Kit.Compiler.Typers.TypeVar
import Kit.Compiler.Unify
import Kit.Compiler.Utils
import Kit.Error
import Kit.Parser
import Kit.Str

{-
  Type checks a non-generic type declaration.
-}
typeType
  :: CompileContext
  -> TypeContext
  -> Module
  -> TypeDefinition TypedExpr ConcreteType
  -> IO TypedDecl
typeType ctx tctx mod def = do
  debugLog ctx
    $  "typing type "
    ++ s_unpack (showTypePath $ typeName def)
    ++ (case typeMonomorph def of
         [] -> ""
         x  -> " monomorph " ++ show x
       )
  typeTypeDefinition
    ctx
    (tctx { tctxSelf = Just $ TypeInstance (typeName def) [] })
    mod
    (TypeInstance (typeName def) [])
    def

typeTypeDefinition
  :: CompileContext
  -> TypeContext
  -> Module
  -> ConcreteType
  -> TypeDefinition TypedExpr ConcreteType
  -> IO TypedDecl
typeTypeDefinition ctx tctx mod selfType def@(TypeDefinition { typeName = name })
  = do
    let s = typeSubtype def
    subtype <- case s of
      Struct { structFields = f } -> do
        when (null f) $ throwk $ TypingError
          ("Can't declare a struct with no fields")
          (typePos def)
        fields <- forM
          f
          (\field -> case varDefault field of
            Just x -> do
              def <- typeExpr ctx tctx mod x
              resolveConstraint
                ctx
                tctx
                (TypeEq
                  (inferredType def)
                  (varType field)
                  "Struct field default value must match the field's type"
                  (varPos field)
                )
              return $ field { varDefault = Just def }
            Nothing -> return field
          )
        return $ s { structFields = fields }
      Union { unionFields = f } -> do
        when (null f) $ throwk $ TypingError
          ("Can't declare a union with no fields")
          (typePos def)
        return s
      -- Enum { enumVariants = variants } -> do
      --   variants <- forM variants $ \variant -> convertEnumVariant
      --     (converter (typeExpr ctx tctx mod) (\_ -> mapType $ follow ctx tctx))
      --     variant
      --   return $ s { enumVariants = variants }
      _ -> return $ typeSubtype def
    return $ DeclType $ def { typeSubtype = subtype }
