module Kit.Compiler.Generators.PatternMatch where

import Control.Exception
import Control.Monad
import Data.IORef
import Data.List
import Data.Maybe
import Kit.Ast
import Kit.Compiler.Binding
import Kit.Compiler.Context
import Kit.Compiler.Generators.FindUnderlyingType
import Kit.NameMangling
import Kit.Compiler.Generators.StringCompare
import Kit.Compiler.Module
import Kit.Compiler.Scope
import Kit.Compiler.TypeContext
import Kit.Compiler.TypedDecl
import Kit.Compiler.TypedExpr
import Kit.Compiler.Unify
import Kit.Compiler.Utils
import Kit.Error
import Kit.HashTable
import Kit.Ir
import Kit.Parser
import Kit.Str

{-
  Given a match pattern and the matched expression, returns a list of
  conditions to check for and var declarations to add to the match body.
-}
patternMatch
  :: CompileContext
  -> Module
  -> (TypedExpr -> IO IrExpr)
  -> TypedExpr
  -> BasicType
  -> IrExpr
  -> IO ([IrExpr], [IrExpr])
patternMatch ctx mod typer pattern t ex = do
  let mergeResults = foldr (\(a, b) (c, d) -> (a ++ c, b ++ d)) ([], [])
  case tExpr pattern of
    EnumInit (TypeInstance tp params) discriminant args -> do
      tctx           <- modTypeContext ctx mod
      resolvedParams <- forM params $ mapType $ follow ctx tctx
      let
        enumDiscriminant = case t of
          BasicTypeSimpleEnum  _ -> ex
          BasicTypeAnonEnum    _ -> ex
          BasicTypeComplexEnum _ -> (IrField ex discriminantFieldName)
          _ -> throwk $ InternalError "Unexpected value used as enum"
                                      (Just $ tPos pattern)
      let enumField fieldName = IrField
            ( IrField (IrField ex variantFieldName)
            $ discriminantMemberName discriminant
            )
            fieldName
      def <- getTypeDefinition ctx tp
      case typeSubtype def of
        Enum { enumVariants = variants } -> do
          let variant = find
                (\variant ->
                  (tpName $ variantName variant) == tpName discriminant
                )
                variants
          case variant of
            Just variant -> do
              args' <-
                forMWithErrors (zip (variantArgs variant) args)
                  $ \(arg, (_, argValue)) -> do
                      modTctx <- modTypeContext ctx mod
                      let tctx = addTypeParams
                            modTctx
                            [ (typeSubPath def $ paramName param, value)
                            | (param, value) <- zip (typeParams def)
                                                    resolvedParams
                            ]
                      t  <- mapType (follow ctx tctx) $ argType arg
                      at <- findUnderlyingType ctx mod (Just $ tPos pattern) t
                      patternMatch ctx
                                   mod
                                   typer
                                   argValue
                                   at
                                   (enumField $ argName arg)
              return $ mergeResults
                ( ( [ IrBinop
                        Eq
                        enumDiscriminant
                        (IrIdentifier $ subPath
                          (monomorphName (variantParent variant) resolvedParams)
                          (tpName $ variantName variant)
                        )
                    ]
                  , []
                  )
                : args'
                )
    -- TODO: tuple destructure
    StructInit t fields -> do
      conds <- forM fields $ \(name, val) -> do
        t <- findUnderlyingType ctx mod (Just $ tPos val) (inferredType val)
        patternMatch ctx mod typer val t (IrField ex name)
      return $ mergeResults conds
    Identifier (Var ([], x)) -> do
      return $ ([], [IrVarDeclaration x t (Just ex)])
    Identifier Hole -> do
      return ([], [])
    Literal (BoolValue   True ) _ -> return ([ex], [])
    Literal (BoolValue   False) _ -> return ([IrPreUnop Invert ex], [])
    Literal (StringValue s    ) _ -> return ([stringCompare ex s], [])
    _                             -> do
      r1 <- typer pattern
      return ([IrBinop Eq ex r1], [])
