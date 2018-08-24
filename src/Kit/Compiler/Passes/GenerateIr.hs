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
import Kit.Compiler.Generators
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
  let converter' = converter (typedToIr ctx mod)
                             (\pos -> findUnderlyingType ctx mod (Just pos))
  let paramConverter = \p -> converter'
  case t of
    DeclType def@(TypeDefinition { typeName = name }) -> do
      debugLog ctx $ "generating IR for " ++ s_unpack name ++ " in " ++ show mod
      -- TODO: params
      converted    <- convertTypeDefinition paramConverter def
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

      converted <- convertFunctionDefinition paramConverter f

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

      converted <- convertVarDefinition converter' v
      return
        [DeclVar $ converted { varName = mangleName (varNamespace v) name }]

    DeclTrait (      TraitDefinition { traitMethods = [] }) -> return []
    DeclTrait trait@(TraitDefinition { traitName = name } ) -> do
      -- FIXME: params
      converted <- convertTraitDefinition paramConverter trait
      -- trait declarations become struct definitions for the box/vtable
      let boxName    = mangleName ((modPath mod) ++ [name]) "box"
      let vtableName = mangleName ((modPath mod) ++ [name]) "vtable"
      let
        traitBox = newTypeDefinition
          { typeName    = boxName
          , typeSubtype = Struct
            { structFields = [ newVarDefinition { varName = valuePointerName
                                                , varType = CPtr BasicTypeVoid
                                                }
                             , newVarDefinition
                               { varName = "__vtable"
                               , varType = CPtr
                                 $ BasicTypeStruct (Just vtableName) []
                               }
                             ]
            }
          }
      let
        vtable = newTypeDefinition
          { typeName    = vtableName
          , typeSubtype = Struct
            { structFields = [ newVarDefinition
                                 { varName = functionName f
                                 , varType = CPtr $ BasicTypeFunction
                                   (functionType f)
                                   ( (vThisArgName, CPtr BasicTypeVoid)
                                   : [ (argName arg, argType arg)
                                     | arg <- functionArgs f
                                     ]
                                   )
                                   (functionVarargs f)
                                 }
                             | f <- traitMethods converted
                             ]
            }
          }
      return [DeclType $ traitBox, DeclType $ vtable]

    DeclImpl (TraitImplementation { implMethods = [] }) -> return []
    DeclImpl i@(TraitImplementation { implTrait = TypeTraitConstraint ((mp, name), params), implFor = ct, implMod = implMod })
      -> do
      -- FIXME: for now we're indexing trait implementations by basic type, but
      -- different concrete types of the same basic type could each have their own
        for <- findUnderlyingType ctx mod (Just $ implPos i) ct
        let implName =
              (mangleName (mp ++ [name, "impl"] ++ implMod)
                          (s_pack $ basicTypeAbbreviation for)
              )
        let vtableName = mangleName ((modPath mod) ++ [name]) "vtable"
        methods <- forM (implMethods i) $ \method -> do
          f' <- convertFunctionDefinition paramConverter method
          let f = implicitifyMethod (CPtr BasicTypeVoid) vThisArgName f'
          let name' =
                (mangleName
                  (  mp
                  ++ [name, "impl"]
                  ++ implMod
                  ++ [s_pack $ basicTypeAbbreviation for]
                  )
                  (functionName f)
                )
          return
            ( name'
            , DeclFunction $ f
              { functionName = name'
              , functionBody = let
                                 v = IrVarDeclaration
                                   thisArgName
                                   for
                                   (Just $ IrPreUnop
                                     Deref
                                     (IrCast (IrIdentifier vThisArgName) (CPtr for))
                                   )
                               in  case functionBody f of
                                     Just (IrBlock x) -> Just (IrBlock (v : x))
                                     Just x           -> Just $ IrBlock [v, x]
                                     _                -> Nothing
              }
            )
        let impl = newVarDefinition
              { varName    = implName
              , varType    = BasicTypeStruct (Just vtableName) []
              , varDefault = Just $ IrStructInit
                (BasicTypeStruct (Just vtableName) [])
                [ (functionName method, IrIdentifier mangledName)
                | ((mangledName, _), method) <- zip methods (implMethods i)
                ]
              }
        return $ (map snd methods) ++ [DeclVar $ impl]

    _ -> return [] -- TODO

maybeTypedToIr ctx mod e = case e of
  Just ex -> do
    t <- typedToIr ctx mod ex
    return $ Just t
  Nothing -> return Nothing

typedToIr :: CompileContext -> Module -> TypedExpr -> IO IrExpr
typedToIr ctx mod e@(TypedExpr { tExpr = et, tPos = pos, inferredType = t }) =
  do
    let converter' = converter
          (typedToIr ctx mod)
          (\pos -> findUnderlyingType ctx mod (Just pos))
    let paramConverter = \p -> converter'
    let r x = typedToIr ctx mod x
    let maybeR x = case x of
          Just x -> do
            r' <- r x
            return $ Just r'
          Nothing -> return Nothing
    f <- findUnderlyingType ctx mod (Just pos) t

    case et of
      (Block children) -> do
        children' <- forM children $ \child -> do
          temps  <- mapM r $ tTemps child
          result <- r child
          return $ temps ++ [result]
        return $ IrBlock $ foldr (++) [] children'
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
        TypeFunction rt args varargs params | not (null params) -> do
          -- generic function
          tctx   <- newTypeContext []
          params <- mapM (mapType $ follow ctx tctx) params
          return $ IrIdentifier $ mangleName namespace $ monomorphName v params
        _ -> return $ IrIdentifier (mangleName namespace v)
      (Method e1 (modPath, typeName) name) -> case t of
        TypeFunction rt args varargs params -> do
          tctx   <- newTypeContext []
          params <- forM params $ mapType $ follow ctx tctx
          return
            $ IrIdentifier
            $ mangleName (modPath ++ [monomorphName typeName params])
            $ name
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
        t1 <- findUnderlyingType ctx mod (Just pos) (inferredType e1)
        t2 <- findUnderlyingType ctx mod (Just pos) (inferredType e2)
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
      (Throw e1         ) -> return $ undefined -- TODO
      (Match e1 cases e2) -> do
        r1        <- r e1
        r2        <- maybeR e2
        matchType <- findUnderlyingType ctx mod (Just pos) (inferredType e1)
        case matchType of
          BasicTypeComplexEnum _ _ -> do
            -- complex match with ADT
            cases' <- forM cases $ \c -> do
              (conditions, exprs) <- patternMatch ctx
                                                  mod
                                                  r
                                                  (matchPattern c)
                                                  matchType
                                                  r1
              body <- r $ matchBody c
              let mergeConditions (h : t) =
                    if null t then h else (IrBinop And h (mergeConditions t))
              return $ (mergeConditions conditions, (IrBlock $ exprs ++ [body]))
            let ifX ((cond, body) : t) =
                  IrIf cond body (if null t then r2 else Just $ ifX t)
            return $ ifX cases'
          BasicTypeTuple _ _ -> do
            -- complex match with tuples
            throwk $ InternalError "Not yet implemented" (Just pos)
          BasicTypeBool -> do
            -- transform into an if statement
            let branchOrDefault b = case b of
                  Just x -> do
                    rx <- r $ matchBody x
                    return $ Just rx
                  Nothing -> return r2
            trueBranch <- branchOrDefault $ find
              (\c -> (tExpr $ matchPattern c) /= (Literal $ BoolValue False))
              cases
            falseBranch <- branchOrDefault $ find
              (\c -> (tExpr $ matchPattern c) /= (Literal $ BoolValue True))
              cases
            case (trueBranch, falseBranch) of
              (Just a, Just b) | a == b -> return $ a
              (Just a, b     )          -> return $ IrIf r1 a b
              (Nothing, Just b) ->
                return $ IrIf (IrPreUnop Invert r1) b Nothing
              (Nothing, Nothing) -> throwk $ InternalError
                "Boolean match with no true or false branches"
                (Just pos)
          _ -> do
            -- simple match
            cases' <- forM cases $ \c -> do
              pattern <- r $ matchPattern c
              body    <- r $ matchBody c
              return (pattern, body)
            def <- maybeR e2
            let canSwitch = case matchType of
                  BasicTypeInt  _         -> True
                  BasicTypeUint _         -> True
                  BasicTypeSimpleEnum _ _ -> True
                  _                       -> False
            if canSwitch
              then return $ IrSwitch r1 cases' def
              else
                let ifX ((a, b) : t) = IrIf
                      (IrBinop Eq r1 a)
                      b
                      (if null t then r2 else Just $ ifX t)
                in  return $ ifX cases'

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
        t1' <- findUnderlyingType ctx mod (Just pos) (inferredType e1)
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
      (EnumDiscriminant x) -> do
        r1       <- r x
        enumType <- findUnderlyingType ctx mod (Just pos) (inferredType x)
        case enumType of
          BasicTypeSimpleEnum  _ _ -> return r1
          BasicTypeComplexEnum _ _ -> return $ IrField r1 discriminantFieldName
      (EnumField x variantName fieldName) -> do
        r1 <- r x
        return
          $ IrField
              ( IrField (IrField r1 variantFieldName)
              $ discriminantMemberName variantName
              )
          $ fieldName
      (TupleInit slots) -> do
        resolvedSlots <- forM
          slots
          (\s -> do
            r1 <- r s
            return r1
          )
        return $ IrTupleInit f resolvedSlots
      (TupleSlot x slot) -> do
        r1 <- r x
        return $ IrField r1 (s_pack $ "__slot" ++ show slot)
      (Box (TraitImplementation { implTrait = TypeTraitConstraint ((modPath, name), params), implFor = for, implMod = implMod }) e1)
        -> do
          r1  <- r e1
          for <- findUnderlyingType ctx mod (Just pos) for
          let structName = (mangleName (modPath ++ [name]) "box")
          -- TODO: fix name
          let implName =
                (mangleName (modPath ++ [name, "impl"] ++ implMod)
                            (s_pack $ basicTypeAbbreviation for)
                )
          return $ IrStructInit
            (BasicTypeStruct (Just structName) [])
            [ (valuePointerName , (IrPreUnop Ref r1))
            , (vtablePointerName, IrPreUnop Ref (IrIdentifier implName))
            ]
      (Box t _) -> throwk $ InternalError
        ("Invalid boxed implementation: " ++ show t)
        (Just pos)
      (BoxedValue trait x) -> do
        box <- r x
        return $ IrField box valuePointerName
      (BoxedVtable trait x) -> do
        box <- r x
        return $ IrPreUnop Deref (IrField box vtablePointerName)
      (SizeOf t) -> do
        t' <- findUnderlyingType ctx mod (Just pos) t
        return $ IrSizeOf t'
      t -> do
        throwk $ InternalError
          ("Unexpected expression in typed AST:\n\n" ++ show t)
          (Just pos)

addHeader :: Module -> FilePath -> Span -> IO ()
addHeader mod fp pos = do
  includes <- readIORef (modIncludes mod)
  unless (elem fp (map fst includes))
    $ modifyIORef (modIncludes mod) (\x -> (fp, pos) : x)
