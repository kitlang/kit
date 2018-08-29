module Kit.Compiler.Generators.TypedExprToIr where

import Control.Exception
import Control.Monad
import Data.IORef
import Data.List
import Data.Maybe
import Kit.Ast
import Kit.Compiler.Binding
import Kit.Compiler.Context
import Kit.Compiler.Generators.FindUnderlyingType
import Kit.Compiler.Generators.NameMangling
import Kit.Compiler.Generators.PatternMatch
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
          tctx   <- modTypeContext ctx mod
          params <- mapM (mapType $ follow ctx tctx) params
          return $ IrIdentifier $ mangleName ctx namespace $ monomorphName
            ctx
            v
            params
        _ -> return $ IrIdentifier (mangleName ctx namespace v)
      (Method e1 (modPath, typeName) name) -> case t of
        TypeFunction rt args varargs params -> do
          tctx   <- modTypeContext ctx mod
          params <- forM params $ mapType $ follow ctx tctx
          return
            $ IrIdentifier
            $ mangleName ctx (modPath ++ [monomorphName ctx typeName params])
            $ name
      (Identifier     (MacroVar v _) _) -> return $ IrIdentifier v
      (TypeAnnotation e1             t) -> throw $ KitError $ BasicError
        ("unexpected type annotation in typed AST")
        (Just pos)
      (PreUnop Ref (TypedExpr { tExpr = This })) -> do
        return $ IrIdentifier thisPtrName
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
            return $ IrCall (IrIdentifier "fmod") [r1, r2]
          32 -> do
            return $ IrCall (IrIdentifier "fmodf") [r1, r2]
          _ -> return $ IrBinop Mod r1 r2
      (Binop Eq (TypedExpr { tExpr = Literal (StringValue s) }) e2) -> do
        r2 <- r e2
        return $ stringCompare r2 s
      (Binop Eq e1 (TypedExpr { tExpr = Literal (StringValue s) })) -> do
        r1 <- r e1
        return $ stringCompare r1 s
      (Binop op e1 e2) -> do
        r1 <- r e1
        r2 <- r e2
        return $ IrBinop op r1 r2
      (For e1@(TypedExpr { tExpr = Identifier (Var id) [] }) (TypedExpr { tExpr = RangeLiteral eFrom eTo }) e3)
        -> do
          t     <- findUnderlyingType ctx mod (Just $ tPos e1) (inferredType e1)
          rFrom <- r eFrom
          rTo   <- r eTo
          r3    <- r e3
          return $ IrFor id t rFrom rTo r3
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
          BasicTypeComplexEnum _ -> do
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
                  a | typeIsIntegral a  -> True
                  BasicTypeSimpleEnum _ -> True
                  BasicTypeAnonEnum   _ -> True
                  _                     -> False
            if canSwitch
              then return $ IrSwitch r1 cases' def
              else
                let ifX ((a, b) : t) = IrIf
                      (case a of
                        IrLiteral (StringValue s) -> stringCompare r1 s
                        _                         -> (IrBinop Eq r1 a)
                      )
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
      (EnumInit (TypeInstance tp p) d args) -> do
        discriminant <- enumDiscriminantName ctx tp p d
        resolvedArgs <- forM args $ \(name, x) -> do
          x <- r x
          return (name, x)
        f' <- case f of
          BasicTypeComplexEnum n -> do
            return $ BasicTypeComplexEnum n
          f -> return f
        return $ IrEnumInit f' discriminant resolvedArgs
      (EnumDiscriminant x) -> do
        r1       <- r x
        enumType <- findUnderlyingType ctx mod (Just pos) (inferredType x)
        case enumType of
          BasicTypeSimpleEnum  _ -> return r1
          BasicTypeAnonEnum    _ -> return r1
          BasicTypeComplexEnum _ -> return $ IrField r1 discriminantFieldName
      (EnumField x variantName fieldName) -> do
        r1 <- r x
        let (TypeInstance tp params) = inferredType x
        variantName <- enumDiscriminantName ctx tp params variantName
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
      (Box (TraitImplementation { implTrait = TypeTraitConstraint ((modPath, traitName), params), implFor = for, implMod = implMod }) e1)
        -> do
          r1     <- r e1
          for    <- findUnderlyingType ctx mod (Just pos) for
          tctx   <- modTypeContext ctx mod
          params <- forM params $ mapType (follow ctx tctx)
          let name       = monomorphName ctx traitName params
          let structName = (mangleName ctx (modPath ++ [name]) "box")
          -- TODO: fix name
          let implName =
                (mangleName ctx
                            (modPath ++ [name, "impl"] ++ implMod)
                            (s_pack $ basicTypeAbbreviation for)
                )
          return $ IrStructInit
            (BasicTypeStruct structName)
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

{-addHeader :: Module -> FilePath -> Span -> IO ()
addHeader mod fp pos = do
  includes <- readIORef (modIncludes mod)
  unless (elem fp (map fst includes))
    $ modifyIORef (modIncludes mod) (\x -> (fp, pos) : x)-}
