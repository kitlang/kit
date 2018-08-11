{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Kit.Compiler.Passes.GenerateIr where

import Control.Exception
import Control.Monad
import Data.IORef
import Data.List
import Data.Maybe
import Kit.Ast
import Kit.Compiler.Binding
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
generateIr
  :: CompileContext -> [(Module, [TypedDecl])] -> IO [(Module, [IrDecl])]
generateIr ctx modContent = forM modContent (generateModuleIr ctx)

generateModuleIr
  :: CompileContext -> (Module, [TypedDecl]) -> IO (Module, [IrDecl])
generateModuleIr ctx (mod, decls) = do
  debugLog ctx $ "generating IR for " ++ show mod
  decls  <- forM decls (generateDeclIr ctx mod)
  tuples <- h_toList (modTuples mod)
  return (mod, [ DeclTuple t | (_, t) <- tuples ] ++ (foldr (++) [] decls))

generateDeclIr :: CompileContext -> Module -> TypedDecl -> IO [IrDecl]
generateDeclIr ctx mod t = do
  let exprConverter = (typedToIr ctx mod)
  let typeConverter = (\pos -> findUnderlyingType ctx mod)
  case t of
    DeclType def@(TypeDefinition { typeName = name }) -> do
      debugLog ctx $ "generating IR for " ++ s_unpack name ++ " in " ++ show mod
      -- TODO: params
      converted <- convertTypeDefinition
        (\_ -> converter exprConverter typeConverter)
        def
      -- TODO: add declarations for instance methods
      staticFields <- forM
        (typeStaticFields def)
        (\field -> generateDeclIr ctx mod
          $ DeclVar (field { varNamespace = (modPath mod) ++ [name] })
        )
      staticMethods <- forM
        (typeStaticMethods def)
        (\method -> generateDeclIr ctx mod $ DeclFunction
          (method { functionNamespace = (modPath mod) ++ [name] })
        )
      instanceMethods <- forM
        (typeMethods def)
        (\method -> generateDeclIr ctx mod $ DeclFunction
          (method { functionNamespace = (modPath mod) ++ [name] })
        )
      return
        $ (DeclType converted)
        : (foldr (++) [] (staticFields ++ staticMethods ++ instanceMethods))

    DeclFunction f@(FunctionDefinition { functionName = name }) -> do
      debugLog ctx
        $  "generating IR for function "
        ++ s_unpack name
        ++ " in "
        ++ show mod

      let isMain =
            (functionName f == "main")
              && (ctxMainModule ctx == modPath mod)
              && not (ctxIsLibrary ctx)

      -- FIXME: params
      converted <- convertFunctionDefinition
        (\params -> converter exprConverter typeConverter)
        f

      if (isMain && functionType converted == BasicTypeVoid)
        then return
          [ DeclFunction $ converted
              { functionName = name
              , functionType = BasicTypeInt 16
              , functionBody = case functionBody converted of
                Just x ->
                  Just
                    $ IrBlock
                        [ x
                        , IrReturn
                        $ Just
                        $ IrLiteral
                        $ IntValue 0
                        $ BasicTypeInt 16
                        ]
                Nothing -> Just
                  (IrReturn $ Just $ IrLiteral $ IntValue 0 $ BasicTypeInt 16)
              }
          ]
        else return
          [ DeclFunction $ converted
              { functionName = mangleName (functionNamespace f) name
              }
          ]

    DeclVar v@(VarDefinition { varName = name }) -> do
      debugLog ctx
        $  "generating IR for var "
        ++ s_unpack name
        ++ " in "
        ++ show mod

      converted <- convertVarDefinition
        (converter exprConverter typeConverter)
        v
      return
        [DeclVar $ converted { varName = mangleName (varNamespace v) name }]

    _ -> return [] -- TODO

{-
  Recursively dereference a high level ConcreteType into a BasicType.
-}
findUnderlyingType :: CompileContext -> Module -> ConcreteType -> IO BasicType
findUnderlyingType ctx mod t = do
  x <- case t of
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
    TypeAnonUnion fields -> do
      fields' <- forM
        fields
        (\(name, t) -> do
          t' <- findUnderlyingType ctx mod t
          return (name, t')
        )
      return $ BasicTypeUnion Nothing fields'
    TypeInstance (modPath, name) params -> do
      definitionMod <- getMod ctx modPath
      binding       <- resolveLocal (modScope definitionMod) name
      case binding of
        Just (Binding { bindingType = TypeBinding (TypeDefinition { typeSubtype = subtype }) })
          -> case subtype of
            Struct { structFields = fields } -> do
              fields <- forM fields $ \field -> do
                t <- findUnderlyingType ctx mod (varType field)
                return (varName field, t)
              return $ BasicTypeStruct (Just name) fields
            Union { unionFields = fields } -> do
              fields <- forM fields $ \field -> do
                t <- findUnderlyingType ctx mod (varType field)
                return (varName field, t)
              return $ BasicTypeUnion (Just name) fields
            enum@(Enum { enumVariants = variants }) -> do
              if enumIsSimple enum
                then return $ BasicTypeSimpleEnum (Just name) $ map
                  variantName
                  variants
                else do
                  variants' <- forM variants $ \variant -> do
                    args <- forM (variantArgs variant) $ \arg -> do
                      t <- findUnderlyingType ctx mod $ argType arg
                      return (argName arg, t)
                    return (variantName variant, args)
                  return $ BasicTypeComplexEnum name variants'
            Abstract{} -> do
              throwk $ InternalError "Not yet implemented" Nothing
        _ -> throwk $ BasicError
          (  "Unexpected missing type definition: "
          ++ (s_unpack $ showTypePath (modPath, name))
          )
          Nothing
      -- typeDef       <- h_lookup (modContents definitionMod) name
      -- TODO
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
    TypeTuple t -> do
      slots <- forM t (findUnderlyingType ctx mod)
      return $ BasicTypeTuple
        (s_pack (basicTypeAbbreviation $ BasicTypeTuple "" slots))
        slots
    _ -> do
      -- TODO: REMOVE
      return $ BasicTypeUnknown

  case x of
    BasicTypeTuple name t -> h_insert (modTuples mod) (s_unpack name) x
    _                     -> return ()

  return x

-- TODO: finding specializations should be done as a separate step
findDefaultType :: CompileContext -> Module -> Int -> IO BasicType
findDefaultType ctx mod id = do
  info <- getTypeVar ctx id
  if null (typeVarConstraints info)
    then throwk $ BasicError
      ("The type of this expression is ambiguous; not enough information to infer a type for type var #"
      ++ show id
      ++ ".\n\nTry adding a type annotation: `expression: Type`"
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
                      Just _ -> acc'
                      _      -> Nothing
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
typedToIr ctx mod e@(TypedExpr { tExpr = et, tPos = pos, inferredType = t }) =
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
      (Using _ e1            ) -> r e1
      (Meta  m e1            ) -> r e1
      (Literal (IntValue v _)) -> do
        return $ IrCast (IrLiteral (IntValue v f)) f
      (Literal (l@(FloatValue v _))) -> case f of
        -- FIXME: this is better handled at the code generation stage now that we have literal types
        BasicTypeFloat 32 ->
          return (IrLiteral (FloatValue (s_concat [v, "f"]) f))
        _ -> return (IrLiteral (FloatValue v f))
      (Literal (StringValue s)) -> return $ IrLiteral (StringValue s)
      (Literal (BoolValue   b)) -> return $ IrLiteral (BoolValue b)
      (This                   ) -> return $ IrIdentifier "__this"
      (Self                   ) -> throw $ KitError $ BasicError
        ("unexpected `Self` in typed AST")
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
      (For   e1 e2 e3) -> return $ undefined -- TODO
      (While e1 e2 d ) -> do
        r1 <- r e1
        r2 <- r e2
        return $ IrWhile r1 r2 d
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
      (Unsafe       e1   ) -> r e1
      (BlockComment s    ) -> return $ IrBlock []
      (RangeLiteral e1 e2) -> throwk
        $ BasicError ("unexpected range literal in typed AST") (Just pos)
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
      (TupleInit slots) -> do
        resolvedSlots <- forM
          slots
          (\s -> do
            r1 <- r s
            return r1
          )
        return $ IrTupleInit f resolvedSlots

addHeader :: Module -> FilePath -> Span -> IO ()
addHeader mod fp pos = do
  includes <- readIORef (modIncludes mod)
  unless (elem fp (map fst includes))
    $ modifyIORef (modIncludes mod) (\x -> (fp, pos) : x)
