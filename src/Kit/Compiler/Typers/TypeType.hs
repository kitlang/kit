module Kit.Compiler.Typers.TypeType where

import Control.Monad
import Data.IORef
import Data.List
import Kit.Ast
import Kit.Compiler.Context
import Kit.Compiler.Module
import Kit.Compiler.Scope
import Kit.Compiler.TypeContext
import Kit.Compiler.TypedDecl
import Kit.Compiler.TypedExpr
import Kit.Compiler.Typers.Base
import Kit.Compiler.Typers.ConvertExpr
import Kit.Compiler.Typers.TypeExpression
import Kit.Compiler.Typers.TypeFunction
import Kit.Compiler.Typers.TypeVar
import Kit.Compiler.Unify
import Kit.Compiler.Utils
import Kit.Error
import Kit.Parser
import Kit.Str

typeTypeDefinition
  :: CompileContext
  -> Module
  -> TypeDefinition TypedExpr ConcreteType
  -> IO (Maybe TypedDecl, Bool)
typeTypeDefinition ctx mod def@(TypeDefinition { typeName = name }) = do
  debugLog ctx $ "typing " ++ s_unpack name ++ " in " ++ show mod
  -- TODO: handle params here
  tctx' <- modTypeContext ctx mod
  let tctx = tctx'
        { tctxTypeParams = [ (paramName param, ()) | param <- typeParams def ]
        }
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
  -- TODO: type methods, variable defaults, fields, enum variants...
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
              mod
              (TypeEq (inferredType def)
                      (varType field)
                      "Struct field default value must match the field's type"
                      (varPos field)
              )
            return $ field { varDefault = Just def }
          Nothing -> return field
        )
      return $ s { structFields = fields }
    _ -> return $ typeSubtype def
  return
    $ ( Just $ DeclType
        (def { typeStaticFields  = staticFields
             , typeStaticMethods = staticMethods
             , typeSubtype       = subtype
             }
        )
      , True
      )
