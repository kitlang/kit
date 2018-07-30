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

{-
  Generates declarations in interediate representation for each typed module.
-}
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
  let exprConverter = (typedToIr ctx mod)
  let typeConverter = (findUnderlyingType ctx mod)
  case t of
    DeclType def@(TypeDefinition { typeName = name }) -> do
      debugLog ctx $ "generating IR for " ++ s_unpack name ++ " in " ++ show mod
      converted <- convertTypeDefinition exprConverter typeConverter def
      addDecl $ DeclType $ converted
      -- TODO: add declarations for instance methods
      forM_
        (typeStaticFields def)
        (\field -> generateDeclIr ctx mod
          $ DeclVar (field { varNamespace = (modPath mod) ++ [name] })
        )
      forM_
        (typeStaticMethods def)
        (\method -> generateDeclIr ctx mod
          $ DeclFunction (method { functionNamespace = (modPath mod) ++ [name] })
        )
    DeclFunction f@(FunctionDefinition { functionName = name, functionArgs = args, functionType = t })
      -> do
        debugLog ctx
          $  "generating IR for function "
          ++ s_unpack name
          ++ " in "
          ++ show mod

        args       <- forM args (convertArgSpec exprConverter typeConverter)
        returnType <- typeConverter t
        converted  <- convertFunctionDefinition exprConverter
                                                typeConverter
                                                args
                                                returnType
                                                f
        addDecl $ DeclFunction $ converted
          { functionName = mangleName (functionNamespace f) name
          }
    DeclVar v@(VarDefinition { varName = name, varType = t }) -> do
      debugLog ctx
        $  "generating IR for var "
        ++ s_unpack name
        ++ " in "
        ++ show mod

      converted <- convertVarDefinition exprConverter typeConverter v
      addDecl $ DeclVar $ converted { varName = mangleName (varNamespace v) name
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
      typeDef       <- resolveLocal (modContents definitionMod) name
      -- TODO
      return $ BasicTypeStruct (Just name) []
    TypeUnion (modPath, name) fieldTypes -> do
      definitionMod <- getMod ctx modPath
      typeDef       <- resolveLocal (modContents definitionMod) name
      -- TODO
      return $ BasicTypeUnion (Just name) []
    TypeEnum tp@(modPath, name) argTypes -> do
      definitionMod <- getMod ctx modPath
      typeDef       <- resolveLocal (modContents definitionMod) name
      case typeDef of
        Just (DeclType (TypeDefinition { typeSubtype = enum@(Enum { enumVariants = variants }) }))
          -> return $ if enumIsSimple enum
            then BasicTypeSimpleEnum (Just name) [] -- we won't care about the variants in this case
            else BasicTypeComplexEnum
              name
              [ ( variantName variant
                , [ (argName arg, BasicTypeUnknown)
                  | arg <- variantArgs variant
                  ]
                )
              | variant <- variants
              ]
        _ -> throwk $ InternalError "Expected enum not found" Nothing
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
        TypeTypeVar id -> if tv == id
          then findDefaultType ctx mod id
          else findUnderlyingType ctx mod known
        _ -> findUnderlyingType ctx mod known
    _ -> do
      -- TODO: REMOVE
      return $ BasicTypeUnknown

-- TODO: finding specializations should be done as a separate step
findDefaultType :: CompileContext -> Module -> Int -> IO (BasicType)
findDefaultType ctx mod id = do
  info <- getTypeVar ctx id
  if null (typeVarConstraints info)
    then throwk $ BasicError
      ("The type of this expression is ambiguous; not enough information to infer a type.\n\nTry adding a type annotation: `expression: Type`"
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
          ++ "\n\nbut no specialization for one of these traits satisfies all of them, so no concrete type can be determined.\n\nTry adding a type annotation: `(myExpression: Type)`"
          )
          (Just $ head $ typeVarPositions info)

maybeTypedToIr ctx mod e = case e of
  Just ex -> do
    t <- typedToIr ctx mod ex
    return $ Just t
  Nothing -> return Nothing

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
      (Identifier (Var v) namespace) -> case t of
        TypeTypeOf x -> throwk $ BasicError
          "Names of types can't be used as runtime values"
          (Just pos)
        _ -> return $ IrIdentifier (mangleName namespace v)
      (Identifier     (MacroVar v _) _) -> return $ IrIdentifier v
      (TypeAnnotation e1             t) -> throw $ KitError $ BasicError
        ("unexpected type annotation in typed AST")
        (Just pos)
      (PreUnop op e1) -> do
        r1 <- r e1
        return $ IrPreUnop op r1
      (PostUnop op e1) -> do
        r1 <- r e1
        return $ IrPostUnop op r1
      (Binop Mod e1 e2) -> do
        -- FIXME: this special handling is C-specific and should be in
        -- GenerateCode, not GenerateIr
        r1 <- r e1
        r2 <- r e2
        t1 <- findUnderlyingType ctx mod (inferredType e1)
        t2 <- findUnderlyingType ctx mod (inferredType e2)
        let maxFloat = foldr
              (\t acc -> case t of
                BasicTypeFloat f -> max f acc
                _                -> acc
              )
              0
              [t1, t2]
        case maxFloat of
          64 -> do
            addHeader mod "math.h" pos
            return $ IrCall (IrIdentifier "fmod") [r1, r2]
          32 -> do
            addHeader mod "math.h" pos
            return $ IrCall (IrIdentifier "fmodf") [r1, r2]
          _ -> return $ IrBinop Mod r1 r2
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
      (Throw      e1   ) -> return $ undefined -- TODO
      (Match e1 cases (e2)) -> do
        -- TODO
        throwk $ InternalError "Not yet implemented" (Just pos)
        -- r1 <- r e1

      (InlineCall e1   ) -> return $ undefined -- TODO
      (Field e1 (Var v)) -> do
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
      (Defer x) -> do
        throwk $ InternalError "Not yet implemented" (Just pos)
      (StructInit t fields) -> do
        resolvedFields <- forM
          fields
          (\(name, e1) -> do
            r1 <- r e1
            return (name, r1)
          )
        return $ IrStructInit f resolvedFields
      (EnumInit t d args) -> do
        resolvedArgs <- forM args r
        return $ IrEnumInit f d resolvedArgs

addHeader :: Module -> FilePath -> Span -> IO ()
addHeader mod fp pos = do
  includes <- readIORef (modIncludes mod)
  if elem fp (map fst includes)
    then return ()
    else modifyIORef (modIncludes mod) (\x -> (fp, pos) : x)
