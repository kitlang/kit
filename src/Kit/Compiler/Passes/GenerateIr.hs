{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Kit.Compiler.Passes.GenerateIr where

  import Control.Exception
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
  import Kit.Compiler.Utils
  import Kit.Error
  import Kit.HashTable
  import Kit.Ir
  import Kit.Parser
  import Kit.Str

  generateIr :: CompileContext -> IO ()
  generateIr ctx = do
    mods <- h_toList $ ctxModules ctx
    forM_ (map snd mods) (generateModuleIr ctx)
    return ()

  generateModuleIr :: CompileContext -> Module -> IO ()
  generateModuleIr ctx mod = do
    bindings <- bindingList $ mod_typed_contents mod
    forM (bindings) (\t -> generateDeclIr ctx mod t)
    return ()

  generateDeclIr :: CompileContext -> Module -> TypedDecl -> IO ()
  generateDeclIr ctx mod t = do
    let addDecl d = modifyIORef (mod_ir mod) (\x -> d : x)
    case t of
      TypedFunction {typedFunctionName = name, typedFunctionReturnType = rt, typedFunctionBody = body, typedFunctionArgs = args, typedFunctionVariadic = varargs} -> do
        rt <- followType ctx rt
        typedArgs <- mapM (\(a, b) -> do t <- followType ctx b; return (a, t)) args
        addDecl $ IrFunction name (BasicTypeFunction rt typedArgs varargs) (typedToIr body)
      _ -> undefined -- TODO

  followType :: CompileContext -> ConcreteType -> IO BasicType
  followType ctx t = do
    case t of
      TypeAtom s -> return $ BasicTypeAtom s
      TypeStruct tp fieldTypes -> do
        -- TODO
        return $ BasicTypeUnknown
      -- TypeEnum TypePath [ConcreteType]
      -- TypeAbstract TypePath [ConcreteType]
      -- TypeTypedef TypePath [ConcreteType]
      -- TypeFunction ConcreteType ConcreteArgs Bool
      TypeBasicType b -> return b
      -- TypePtr ConcreteType
      -- TypeArr ConcreteType (Maybe Int)
      -- TypeEnumConstructor TypePath ConcreteArgs
      -- TypeLvalue ConcreteType
      -- TypeRange
      -- TypeTraitPointer TypePath
      TypeTypeVar tv -> do
        tctx <- newTypeContext [] -- TODO...
        known <- knownType ctx tctx t
        case known of
          TypeTypeVar tv' ->
            if tv == tv'
              then throw $ Errs [err ValidationError ("unresolved type variable: " ++ (show tv))]
              else followType ctx known
          _ -> followType ctx known

  typedToIr :: TypedExpr -> IrExpr
  typedToIr e@(TypedExpr {texpr = et, tPos = pos, inferredType = t}) =
    let maybeTypedToIr x = case x of {Just x -> Just $ typedToIr x; Nothing -> Nothing} in
    case et of
    (Block children) -> IrBlock (map typedToIr children)
    (Meta m e1) -> typedToIr e1
    (Literal l) -> IrLiteral l -- TODO: ??
    (This) -> IrIdentifier "__this"
    (Self) -> throw $ Errs [errp ValidationError ("unexpected Self in typed AST") (Just pos)]
    (Lvalue (Var v)) -> IrIdentifier v
    (Lvalue (MacroVar v)) -> throw $ Errs [errp ValidationError ("unexpected macro var (" ++ (s_unpack v) ++ ") in typed AST") (Just pos)]
    (EnumConstructor s) -> undefined -- TODO
    (TypeAnnotation e1 t) -> throw $ Errs [errp ValidationError ("unexpected type annotation in typed AST") (Just pos)]
    (PreUnop op e1) -> IrPreUnop op (typedToIr e1)
    (PostUnop op e1) -> IrPostUnop op (typedToIr e1)
    (Binop op e1 e2) -> IrBinop op (typedToIr e1) (typedToIr e2)
    (For e1 e2 e3) -> undefined -- TODO
    (While e1 e2) -> IrWhile (typedToIr e1) (typedToIr e2)
    (If e1 e2 e3) -> IrIf (typedToIr e1) (typedToIr e2) (maybeTypedToIr e3)
    (Continue) -> IrContinue
    (Break) -> IrBreak
    (Return e1) -> IrReturn (maybeTypedToIr e1)
    (Throw e1) -> undefined -- TODO
    (Match e1 cases (e2)) -> undefined -- TODO
    (InlineCall e1) -> undefined -- TODO
    (Field e1 (Var v)) -> IrField (typedToIr e1) v
    (Field e1 (MacroVar v)) -> throw $ Errs [errp ValidationError ("unexpected macro var (" ++ (s_unpack v) ++ ") in typed AST") (Just pos)]
    (ArrayAccess e1 e2) -> undefined -- TODO
    (Call e1 args) -> IrCall (typedToIr e1) (map typedToIr args)
    (Cast e1 t) -> undefined -- TODO
    (TokenExpr tc) -> undefined -- TODO
    (Unsafe e1) -> throw $ Errs [errp ValidationError ("unexpected `unsafe` in typed AST") (Just pos)]
    (BlockComment s) -> IrBlock []
    (New t args) -> undefined -- TODO
    (Copy e1) -> undefined -- TODO
    (Delete e1) -> undefined -- TODO
    (Move e1) -> undefined -- TODO
    (LexMacro s t) -> throw $ Errs [errp ValidationError ("unexpected lexical macro invocation in typed AST") (Just pos)]
    (RangeLiteral e1 e2) -> throw $ Errs [errp ValidationError ("unexpected range literal in typed AST") (Just pos)]
    (VectorLiteral items) -> IrCArrLiteral (map typedToIr items)
    (VarDeclaration vardef) -> undefined -- TODO
