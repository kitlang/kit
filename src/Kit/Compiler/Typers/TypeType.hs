module Kit.Compiler.Typers.TypeType where

import Control.Monad
import Data.IORef
import Data.List
import Kit.Ast
import Kit.Compiler.Binding
import Kit.Compiler.Context
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
  -> Module
  -> TypeDefinition TypedExpr ConcreteType
  -> IO (Maybe TypedDecl, Bool)
typeType ctx mod def = do
  debugLog ctx $ "typing " ++ s_unpack (typeName def) ++ " in " ++ show mod
  tctx    <- modTypeContext ctx mod
  binding <- scopeGet (modScope mod) (typeName def)
  typeTypeDefinition
    ctx
    (tctx { tctxSelf = Just $ TypeInstance (modPath mod, typeName def) [] })
    mod
    (bindingConcrete binding)
    def

{-
  Type checks a specific monomorph of a generic type, with a known set of
  parameters. By this point the final types of the parameters must be known.
-}
typeTypeMonomorph
  :: CompileContext
  -> Module
  -> TypeDefinition TypedExpr ConcreteType
  -> [ConcreteType]
  -> IO (Maybe TypedDecl, Bool)
typeTypeMonomorph ctx mod def params = do
  debugLog ctx
    $  "generating type monomorph for "
    ++ s_unpack (typeName def)
    ++ " in "
    ++ show mod
  let selfType = TypeInstance (modPath mod, typeName def) params
  tctx' <- modTypeContext ctx mod
  let tctx = (addTypeParams
               tctx'
               [ (typeSubPath (modPath mod) def $ paramName param, ct)
               | (param, ct) <- zip (typeParams def) params
               ]
             ) { tctxSelf = Just selfType
               }
  monomorph <- followType ctx tctx (modPath mod) def
  typeTypeDefinition ctx tctx mod selfType monomorph

typeTypeDefinition
  :: CompileContext
  -> TypeContext
  -> Module
  -> ConcreteType
  -> TypeDefinition TypedExpr ConcreteType
  -> IO (Maybe TypedDecl, Bool)
typeTypeDefinition ctx tctx mod selfType def@(TypeDefinition { typeName = name })
  = do
    let r = typeExpr ctx tctx mod
    staticFields <- forM
      (typeStaticFields def)
      (\field -> do
        (typed, complete) <- typeVarDefinition ctx tctx mod field
        return typed
      )
    staticMethods <- forM
      (typeStaticMethods def)
      (\method -> do
        (typed, complete) <- typeFunctionDefinition ctx tctx mod method
        return typed
      )
    instanceMethods <- forM
      (typeMethods def)
      (\method -> do
        let tctx' = tctx { tctxThis = Just selfType }
        (typed, complete) <- typeFunctionDefinition ctx tctx' mod method
        -- revise self type in instance methods
        -- return $ reimplicitify (TypePtr selfType) typed
        return typed
      )
    let s = typeSubtype def
    subtype <- case s of
      Struct { structFields = f } -> do
        fields <- forM
          f
          (\field -> case varDefault field of
            Just x -> do
              def <- r x
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
      -- Enum { enumVariants = variants } -> do
      --   variants <- forM variants $ \variant -> convertEnumVariant
      --     (converter (typeExpr ctx tctx mod) (\_ -> mapType $ follow ctx tctx))
      --     variant
      --   return $ s { enumVariants = variants }
      _ -> return $ typeSubtype def
    return
      $ ( Just $ DeclType
          (def { typeStaticFields  = staticFields
               , typeStaticMethods = staticMethods
               , typeMethods       = instanceMethods
               , typeSubtype       = subtype
               }
          )
        , True
        )
