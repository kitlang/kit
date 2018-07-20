{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Kit.Compiler.Passes.GenerateIr where

import Control.Exception
import Control.Monad
import Data.IORef
import Data.List
import Data.Maybe
import Kit.Ast
import Kit.Compiler.Context
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

generateIr :: CompileContext -> IO ()
generateIr ctx = do
  mods <- ctxSourceModules ctx
  forM_ mods (generateModuleIr ctx)
  return ()

generateModuleIr :: CompileContext -> Module -> IO ()
generateModuleIr ctx mod = do
  debugLog ctx $ "generating IR for " ++ show mod
  bindings <- bindingList $ modTypedContents mod
  forM (bindings) (\t -> generateDeclIr ctx mod t)
  return ()

generateDeclIr :: CompileContext -> Module -> TypedDecl -> IO ()
generateDeclIr ctx mod t = do
  let addDecl d = modifyIORef (modIr mod) (\x -> d : x)
  case t of
    TypedFunction f@(FunctionDefinition { functionName = name, functionType = rt, functionBody = Just body, functionArgs = args, functionVarargs = varargs })
      -> do
        rt        <- findUnderlyingType ctx mod rt
        typedArgs <- mapM
          (\arg -> do
            t <- findUnderlyingType ctx mod (argType arg)
            return $ newArgSpec { argName    = argName arg
                                , argType    = t
                                , argDefault = argDefault arg
                                }
          )
          args
        body' <- typedToIr ctx mod body
        addDecl $ IrFunction $ ((convertFunctionDefinition f) :: IrFunction)
          { functionName = name
          , functionType = (BasicTypeFunction
                             rt
                             [ (argName arg, argType arg) | arg <- typedArgs ]
                             varargs
                           )
          , functionBody = Just body'
          }
    _ -> undefined -- TODO

{-
  Recursively dereference a high level ConcreteType into a BasicType.
-}
findUnderlyingType :: CompileContext -> Module -> ConcreteType -> IO BasicType
findUnderlyingType ctx mod t = do
  case t of
    TypeBasicType b       -> return b
    TypeAtom              -> return $ BasicTypeAtom
    TypeAnonStruct fields -> do
      fields' <- forM
        fields
        (\(name, t) -> do
          t' <- findUnderlyingType ctx mod t
          return (name, t')
        )
      return $ BasicTypeStruct Nothing fields'
    TypeStruct (modPath, name) fieldTypes -> do
      definitionMod <- getMod ctx modPath
      typeDef       <- resolveLocal (modDefinitions definitionMod) name
      -- TODO
      return $ BasicTypeStruct (Just name) []
    -- TypeEnum TypePath [ConcreteType]
    -- TypeAbstract TypePath [ConcreteType]
    -- TypeTypedef TypePath [ConcreteType]
    -- TypeFunction ConcreteType ConcreteArgs Bool
    -- TypePtr ConcreteType
    -- TypeArr ConcreteType (Maybe Int)
    -- TypeEnumConstructor TypePath ConcreteArgs
    -- TypeIdentifier ConcreteType
    -- TypeRange
    -- TypeTraitPointer TypePath
    TypePtr t -> do
      t' <- findUnderlyingType ctx mod t
      return $ CPtr t'
    TypeTypeVar tv -> do
      tctx  <- newTypeContext [] -- TODO...
      known <- knownType ctx tctx mod t
      case known of
        TypeTypeVar (tv'@(TypeVar id)) -> if tv == tv'
          then findDefaultType ctx mod id
          else findUnderlyingType ctx mod known
        _ -> findUnderlyingType ctx mod known
    _ -> do
      -- TODO: REMOVE
      return $ BasicTypeUnknown

findDefaultType :: CompileContext -> Module -> Int -> IO (BasicType)
findDefaultType ctx mod id = do
  info <- getTypeVar ctx id
  if null (typeVarConstraints info)
    then throwk $ BasicError
      ("Couldn't determine the type of this expression. Do you need a type annotation?"
      )
      (Just $ head $ typeVarPositions info)
    else do
      tctx <- newTypeContext []
      let constraints = typeVarConstraints info
      defaults <- mapM (h_lookup (ctxTraitSpecializations ctx))
                       (map (fst . fst) constraints)
      let specializations = catMaybes defaults
      specialization <- foldM
        (\acc (tp, _) -> do
          spec <- resolveType ctx tctx mod tp
          case acc of
            Just _  -> return acc
            Nothing -> do
              meetConstraints <- foldM
                (\acc' c -> case acc' of
                  Just _ -> do
                    -- FIXME: params
                    result <- unify ctx
                                    tctx
                                    mod
                                    spec
                                    (TypeTraitConstraint (c, []))
                    return $ case result of
                      TypeConstraintSatisfied -> acc'
                      _                       -> Nothing
                  Nothing -> do
                    return acc'
                )
                (Just spec)
                (map (fst . fst) constraints)
              case meetConstraints of
                Just _  -> return meetConstraints
                Nothing -> return Nothing
        )
        Nothing
        specializations
      case specialization of
        Just t -> do
          tctx <- newTypeContext []
          findUnderlyingType ctx mod t
        _ -> throwk $ BasicError
          ("This expression has constraints: \n\n"
          ++ (intercalate
               "\n"
               [ "  - " ++ s_unpack (showTypePath c) ++ " (" ++ reason ++ ")"
               | ((c, _), (reason, _)) <- constraints
               ]
             )
          ++ "\n\nbut no specialization for these traits satisfies all of the constraints, so no concrete type can be determined.\n\nTry adding a type annotation."
          )
          (Just $ head $ typeVarPositions info)

typedToIr :: CompileContext -> Module -> TypedExpr -> IO IrExpr
typedToIr ctx mod e@(TypedExpr { texpr = et, tPos = pos, inferredType = t }) =
  do
    let r x = typedToIr ctx mod x
    let maybeR x = case x of
          Just x -> do
            r' <- r x
            return $ Just r'
          Nothing -> return Nothing
    f <- findUnderlyingType ctx mod t
    case et of
      (Block children) -> do
        children' <- mapM r children
        return $ IrBlock children'
      (Meta m e1               ) -> r e1
      (Literal (l@(IntValue _))) -> do
        return $ IrCast (IrLiteral l) f
      (Literal (l@(FloatValue v))) -> case f of
        BasicTypeFloat 32 ->
          return (IrLiteral (FloatValue (s_concat [v, "f"])))
        _ -> return (IrLiteral l)
      (Literal l) -> do
        return $ IrLiteral l
      (This) -> return $ IrIdentifier "__this"
      (Self) -> throw $ KitError $ BasicError
        ("unexpected Self in typed AST")
        (Just pos)
      (Identifier (Var v) mangle) ->
        return $ IrIdentifier (mangleName mangle v)
      (Identifier (MacroVar v _) _) -> throw $ KitError $ BasicError
        ("unexpected macro var (" ++ (s_unpack v) ++ ") in typed AST")
        (Just pos)
      (TypeAnnotation e1 t) -> throw $ KitError $ BasicError
        ("unexpected type annotation in typed AST")
        (Just pos)
      (PreUnop op e1) -> do
        r1 <- r e1
        return $ IrPreUnop op r1
      (PostUnop op e1) -> do
        r1 <- r e1
        return $ IrPostUnop op r1
      (Binop op e1 e2) -> do
        r1 <- r e1
        r2 <- r e2
        return $ IrBinop op r1 r2
      (For e1 e2 e3) -> return $ undefined -- TODO
      (While e1 e2 ) -> do
        r1 <- r e1
        r2 <- r e2
        return $ IrWhile r1 r2
      (If e1 e2 e3) -> do
        r1 <- r e1
        r2 <- r e2
        r3 <- maybeR e3
        return $ IrIf r1 r2 r3
      (Continue ) -> return $ IrContinue
      (Break    ) -> return $ IrBreak
      (Return e1) -> do
        r1 <- maybeR e1
        return $ IrReturn r1
      (Throw e1           ) -> return $ undefined -- TODO
      (Match e1 cases (e2)) -> return $ undefined -- TODO
      (InlineCall e1      ) -> return $ undefined -- TODO
      (Field e1 (Var v)   ) -> do
        r1 <- r e1
        return $ IrField r1 v
      (Field e1 (MacroVar v _)) -> throwk $ InternalError
        ("unexpected macro var (" ++ (s_unpack v) ++ ") in typed AST")
        (Just pos)
      (ArrayAccess e1 e2  ) -> return $ undefined -- TODO
      (Call        e1 args) -> do
        r1    <- r e1
        args' <- mapM r args
        return $ IrCall r1 args'
      (Cast e1 t2) -> do
        r1  <- r e1
        t1' <- findUnderlyingType ctx mod (inferredType e1)
        return $ if t1' == f then r1 else IrCast r1 f
      (TokenExpr tc) -> return $ undefined -- TODO
      (Unsafe    e1) -> return $ throwk $ BasicError
        ("unexpected `unsafe` in typed AST")
        (Just pos)
      (BlockComment s) -> return $ IrBlock []
      (New t args    ) -> return $ undefined -- TODO
      (Copy   e1     ) -> return $ undefined -- TODO
      (Delete e1     ) -> return $ undefined -- TODO
      (Move   e1     ) -> return $ undefined -- TODO
      (LexMacro s t  ) -> return $ throwk $ BasicError
        ("unexpected lexical macro invocation in typed AST")
        (Just pos)
      (RangeLiteral e1 e2) ->
        throwk $ BasicError ("unexpected range literal in typed AST") (Just pos)
      (VectorLiteral items) -> do
        items' <- mapM r items
        return $ IrCArrLiteral items'
      (VarDeclaration (Var name) ts def) -> do
        def <- maybeR def
        return $ IrVarDeclaration name f def
      (VarDeclaration (MacroVar v _) _ _) -> do
        throwk $ BasicError
          ("unexpected macro var (" ++ (s_unpack v) ++ ") in typed AST")
          (Just pos)
