module Kit.Compiler.Typers.TypeExpression.TypeStructInit where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.List
import Data.Maybe
import Kit.Ast
import Kit.Compiler.Binding
import Kit.Compiler.Context
import Kit.Compiler.Module
import Kit.Compiler.Scope
import Kit.Compiler.TypeContext
import Kit.Compiler.TypedExpr
import Kit.Compiler.Typers.AutoRefDeref
import Kit.Compiler.Typers.ExprTyper
import Kit.Compiler.Typers.TypeExpression.ExprToType
import Kit.Compiler.Typers.TypeExpression.TypeVarBinding
import Kit.Compiler.Unify
import Kit.Compiler.Utils
import Kit.Error
import Kit.Str

typeStructInit :: SubTyper
typeStructInit (TyperUtils { _r = r, _tryRewrite = tryRewrite, _resolve = resolve, _typeExpr = typeExpr }) ctx tctx mod ex@(TypedExpr { tExpr = et, tPos = pos })
  = case et of
    (StructInit structType@(TypeInstance tp p) fields) -> do
      let
        findStruct (TypeInstance tp p) tctx = do
          params <- makeGeneric ctx tp pos p
          tctx <- genericTctx ctx tctx pos (TypeInstance tp $ map snd params)
          params <- forMWithErrors (map snd params) $ mapType $ follow ctx tctx
          structDef <- getTypeDefinition ctx tp
          case typeSubtype structDef of
            Abstract { abstractUnderlyingType = parent@(TypeInstance tp p) } ->
              do
                parent <- mapType (follow ctx tctx) parent
                findStruct parent tctx

            Struct { structFields = structFields } -> do
              let providedNames = map fst fields
              let fieldNames    = map (tpName . varName) structFields
              let extraNames    = providedNames \\ fieldNames
              -- TODO: check for duplicate fields
              -- check for extra fields
              unless (null extraNames) $ throwk $ TypingError
                (  "Struct "
                ++ s_unpack (showTypePath tp)
                ++ " has the following extra fields:\n\n"
                ++ intercalate
                     "\n"
                     [ "  - `" ++ s_unpack name ++ "`" | name <- extraNames ]
                ++ "\n\nRemove these fields or correct any typos."
                )
                pos
              let nonProvidedNames = fieldNames \\ providedNames
              typedFields <- forMWithErrors
                (structFields)
                (\field -> do
                  fieldType <- mapType (follow ctx tctx) $ varType field
                  let
                    provided =
                      find (\(name, _) -> name == tpName (varName field)) fields
                  case provided of
                    Just (name, value) -> do
                      value <- typeExpr ctx tctx mod value
                      return $ Just ((name, value), fieldType)
                    Nothing -> case varDefault field of
                      Just fieldDefault -> do
                        fieldDefault <- typeExpr ctx tctx mod fieldDefault
                        return $ Just
                          ((tpName $ varName field, fieldDefault), fieldType)
                      Nothing -> case tctxState tctx of
                        TypingPattern -> return Nothing
                        _             -> throwk $ TypingError
                          (  "Struct "
                          ++ s_unpack (showTypePath tp)
                          ++ " is missing field "
                          ++ s_unpack (showTypePath $ varName field)
                          ++ ", and no default value is provided."
                          )
                          pos
                )
              typedFields <- forMWithErrors
                (catMaybes typedFields)
                (\((name, expr), fieldType) -> do
                  r1        <- r expr
                  converted <- tryAutoRefDeref ctx tctx fieldType r1
                  resolveConstraint ctx tctx $ TypeEq
                    fieldType
                    (inferredType converted)
                    "Struct field values must match the declared struct field type"
                    (tPos r1)
                  return (name, converted)
                )
              structType <- mapType (follow ctx tctx) $ TypeInstance tp params
              return $ (makeExprTyped (StructInit structType typedFields)
                                      structType
                                      pos
                       )
                { tIsLvalue = True
                , tIsLocal  = True
                }

            x -> throwk $ TypingError
              ("Type " ++ s_unpack (showTypePath tp) ++ " isn't a struct")
              pos

      result <- findStruct structType tctx
      return $ result { inferredType = structType }
