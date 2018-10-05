module Kit.Compiler.Ir.ExprToIr where

import Control.Exception
import Control.Monad
import Data.Function
import Data.IORef
import Data.List
import Data.Maybe
import Kit.Ast
import Kit.Compiler.Context
import Kit.Compiler.Ir.FindUnderlyingType
import Kit.Compiler.Ir.PatternMatchToIr
import Kit.Compiler.Ir.StringCompare
import Kit.Compiler.Module
import Kit.Compiler.TypeContext
import Kit.Compiler.TypedExpr
import Kit.Error
import Kit.HashTable
import Kit.Ir
import Kit.NameMangling
import Kit.Str

data IrTempInfo = IrTempInfo {
  irTempType :: BasicType,
  irTempExpr :: Maybe IrExpr,
  irTempOrder :: Int
}

data IrContext = IrContext {
  ictxTemps :: HashTable IrExpr Str,
  ictxTempInfo :: HashTable Int IrTempInfo,
  ictxNextTempId :: IORef Int,
  ictxChildNo :: Int
}

newIrContext :: IO IrContext
newIrContext = do
  temps      <- h_new
  tempInfo   <- h_new
  nextTempId <- newIORef 1
  return $ IrContext
    { ictxTemps      = temps
    , ictxTempInfo   = tempInfo
    , ictxNextTempId = nextTempId
    , ictxChildNo    = 0
    }

makeIrTempVar :: IrContext -> IO Int
makeIrTempVar ictx = do
  next <- readIORef $ ictxNextTempId ictx
  modifyIORef (ictxNextTempId ictx) ((+) 1)
  return next

irTempName :: Int -> Str
irTempName i = s_concat ["__tmp", s_pack $ show i]

maybeTypedToIr ctx ictx mod e = case e of
  Just ex -> do
    t <- typedToIr ctx ictx mod ex
    return $ Just t
  Nothing -> return Nothing

typedToIr :: CompileContext -> IrContext -> Module -> TypedExpr -> IO IrExpr
typedToIr ctx ictx mod e@(TypedExpr { tExpr = et, tPos = pos, inferredType = t })
  = do
    let converter' = converter
          (typedToIr ctx ictx mod)
          (\pos -> findUnderlyingType ctx mod (Just pos))
    let paramConverter = \p -> converter'
    let r x = typedToIr ctx ictx mod x
    let maybeR x = case x of
          Just x -> do
            r' <- r x
            return $ Just r'
          Nothing -> return Nothing
    f <- findUnderlyingType ctx mod (Just pos) t

    case et of
      (Block children) -> do
        ictx     <- newIrContext
        children <- forMWithErrors (zip [0 ..] children)
          $ \(n, child) -> typedToIr ctx (ictx { ictxChildNo = n }) mod child
        lastId    <- readIORef (ictxNextTempId ictx)
        tempDecls <- forM [1 .. lastId - 1] $ \i -> do
          temp@(IrTempInfo { irTempType = tempType, irTempExpr = tempDefault, irTempOrder = tempOrder }) <-
            h_get (ictxTempInfo ictx) i
          return
            $ ( tempOrder * 2
              , IrVarDeclaration (irTempName i) tempType tempDefault
              )
        children <- return
          [ (n * 2 + 1, child) | (n, child) <- zip [0 ..] children ]
        allChildren <-
          return $ map snd $ sortBy (compare `on` fst) $ tempDecls ++ children
        -- interleave temp decls with block children
        return $ IrBlock $ allChildren
      (Temp x) -> do
        t        <- findUnderlyingType ctx mod (Just pos) $ inferredType x
        x        <- r x
        existing <- h_lookup (ictxTemps ictx) x
        case existing of
          Just s  -> return $ IrIdentifier ([], s)
          Nothing -> do
            nextId <- makeIrTempVar ictx
            h_insert (ictxTemps ictx) x (irTempName nextId)
            if tIsLvalue e
              then do
                h_insert (ictxTempInfo ictx) nextId $ IrTempInfo
                  { irTempType  = t
                  , irTempExpr  = Just x
                  , irTempOrder = ictxChildNo ictx
                  }
                return $ IrIdentifier ([], irTempName nextId)
              else do
                h_insert (ictxTempInfo ictx) nextId $ IrTempInfo
                  { irTempType  = t
                  , irTempExpr  = Nothing
                  , irTempOrder = ictxChildNo ictx
                  }
                return $ IrBinop Assign (IrIdentifier ([], irTempName nextId)) x
      (Using   _            e1) -> r e1
      (Meta    m            e1) -> r e1
      (Literal (IntValue v) _ ) -> do
        return $ IrCast (IrLiteral (IntValue v) f) f
      (Literal l@(FloatValue v) _) -> case f of
        -- FIXME: this is better handled at the code generation stage now that we have literal types
        BasicTypeFloat 32 ->
          return (IrLiteral (FloatValue (s_concat [v, "f"])) f)
        _ -> return (IrLiteral (FloatValue v) f)
      (Literal (StringValue s) _) -> return $ IrLiteral (StringValue s) f
      (Literal (BoolValue   b) _) -> return $ IrLiteral (BoolValue b) f
      (This) -> return $ IrPreUnop Deref (IrIdentifier ([], thisPtrName))
      (Self                     ) -> throw $ KitError $ BasicError
        ("unexpected `Self` in typed AST")
        (Just pos)
      (Identifier (Var v)) -> case t of
        TypeTypeOf x _ -> throwk $ BasicError
          "Names of types can't be used as runtime values"
          (Just pos)
        TypeFunction rt args varargs params | not (null params) -> do
          -- generic function
          tctx   <- modTypeContext ctx mod
          params <- mapMWithErrors (mapType $ follow ctx tctx) params
          return $ IrIdentifier $ monomorphName v params
        _ -> return $ IrIdentifier v
      (Method tp params name) -> do
        tctx   <- modTypeContext ctx mod
        params <- forMWithErrors params $ mapType $ follow ctx tctx
        t      <- mapType (follow ctx tctx) t
        case t of
          TypeFunction rt args varargs _ -> do
            return $ IrIdentifier $ subPath (monomorphName tp params) $ name
          x -> throwk
            $ InternalError ("Unexpected method type: " ++ show x) (Just pos)
      (Identifier (MacroVar v _)) -> return $ IrIdentifier ([], v)
      (TypeAnnotation e1 t      ) -> throw $ KitError $ BasicError
        ("unexpected type annotation in typed AST")
        (Just pos)
      (PreUnop Ref (TypedExpr { tExpr = This })) -> do
        return $ IrIdentifier ([], thisPtrName)
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
            return $ IrCall (IrIdentifier ([], "fmod")) [r1, r2]
          32 -> do
            return $ IrCall (IrIdentifier ([], "fmodf")) [r1, r2]
          _ -> return $ IrBinop Mod r1 r2
      (Binop Eq (TypedExpr { tExpr = Literal (StringValue s) _ }) e2) -> do
        r2 <- r e2
        return $ stringCompare r2 s
      (Binop Eq e1 (TypedExpr { tExpr = Literal (StringValue s) _ })) -> do
        r1 <- r e1
        return $ stringCompare r1 s
      (Binop Assign e1 (TypedExpr { tExpr = ArrayLiteral values })) -> do
        r1     <- r e1
        values <- mapM r values
        -- TODO: could use appropriately sized int type for index
        return $ IrBlock
          [ IrBinop Assign
                    (IrArrayAccess r1 (IrLiteral (IntValue i) BasicTypeCSize))
                    val
          | (i, val) <- zip [0 ..] values
          ]
      (Binop op e1 e2) -> do
        r1 <- r e1
        r2 <- r e2
        return $ IrBinop op r1 r2
      (For e1@(TypedExpr { tExpr = Identifier (Var id) }) (TypedExpr { tExpr = RangeLiteral eFrom eTo }) e3)
        -> do
          t     <- findUnderlyingType ctx mod (Just $ tPos e1) (inferredType e1)
          rFrom <- r eFrom
          rTo   <- r eTo
          r3    <- r e3
          return $ IrFor (tpName id) t rFrom rTo r3
      (While e1 e2 d) -> do
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
        let complexMatch = case matchType of
              BasicTypeComplexEnum _  -> True
              BasicTypeTuple _ _      -> True
              BasicTypeStruct _       -> True
              BasicTypeAnonStruct _ _ -> True
              BasicTypeUnion _        -> True
              BasicTypeAnonUnion _ _  -> True
              _                       -> False
        case matchType of
          _ | complexMatch -> do
            -- complex match with ADT
            cases' <- forMWithErrors cases $ \c -> do
              (conditions, exprs) <- patternMatch ctx
                                                  mod
                                                  r
                                                  (matchPattern c)
                                                  matchType
                                                  r1
              body <- r $ matchBody c
              let mergeConditions x = if null x
                    then (IrLiteral (BoolValue True) BasicTypeBool)
                    else
                      let (h, t) = (head x, tail x)
                      in  if null t
                            then h
                            else (IrBinop And h (mergeConditions t))
              return $ (mergeConditions conditions, (IrBlock $ exprs ++ [body]))
            let ifX ((cond, body) : t) =
                  IrIf cond body (if null t then r2 else Just $ ifX t)
            return $ ifX cases'
          BasicTypeBool -> do
            -- transform into an if statement
            let branchOrDefault b = case b of
                  Just x -> do
                    rx <- r $ matchBody x
                    return $ Just rx
                  Nothing -> return r2
            trueBranch <- branchOrDefault $ find
              (\c -> case (tExpr $ matchPattern c) of
                Literal (BoolValue True) _ -> True
                _                          -> False
              )
              cases
            falseBranch <- branchOrDefault $ find
              (\c -> case (tExpr $ matchPattern c) of
                Literal (BoolValue False) _ -> True
                _                           -> False
              )
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
            cases' <- forMWithErrors cases $ \c -> do
              pattern <- r $ matchPattern c
              body    <- r $ matchBody c
              return (pattern, body)
            def <- maybeR e2
            let ifX ((a, b) : t) = IrIf
                  (case a of
                    IrLiteral (StringValue s) _ -> stringCompare r1 s
                    _                           -> (IrBinop Eq r1 a)
                  )
                  b
                  (if null t then r2 else Just $ ifX t)
            return $ ifX cases'

      (InlineCall e1   ) -> return $ undefined -- TODO
      (Field e1 (Var v)) -> do
        r1 <- r e1
        return $ IrField r1 (tpName v)
      (Field e1 (MacroVar v _)) -> throwk $ InternalError
        ("unexpected macro var (" ++ (s_unpack v) ++ ") in typed AST")
        (Just pos)
      (ArrayAccess e1 e2) -> do
        r1 <- r e1
        r2 <- r e2
        return $ IrArrayAccess r1 r2
      (Call e1 args) -> do
        r1    <- r e1
        args' <- mapMWithErrors r args
        return $ IrCall r1 args'
      (Cast e1 t2) -> do
        r1  <- r e1
        t1' <- findUnderlyingType ctx mod (Just pos) (inferredType e1)
        return $ if t1' == f then r1 else IrCast r1 f
      (Unsafe       e1   ) -> r e1
      (BlockComment s    ) -> return $ IrBlock []
      (RangeLiteral e1 e2) -> throwk
        $ BasicError ("unexpected range literal in typed AST") (Just pos)
      (ArrayLiteral items) -> do
        items' <- mapMWithErrors r items
        let contentType = case f of
              CArray t _ -> t
              _          -> throwk $ BasicError
                ("unexpected array literal type: " ++ show f)
                (Just pos)
        return $ IrCArrLiteral items' contentType
      (VarDeclaration (Var name) ts def) -> do
        def <- maybeR def
        return $ IrVarDeclaration (tpName name) f def
      (VarDeclaration (MacroVar v _) _ _) -> do
        throwk $ BasicError
          ("unexpected macro var (" ++ (s_unpack v) ++ ") in typed AST")
          (Just pos)
      (Defer x) -> do
        throwk $ InternalError "Not yet implemented" (Just pos)
      (StructInit t fields) -> do
        resolvedFields <- forMWithErrors
          fields
          (\(name, e1) -> do
            r1 <- r e1
            return (name, r1)
          )
        return $ IrStructInit f resolvedFields
      (EnumInit (TypeInstance tp p) discriminant args) -> do
        tctx           <- modTypeContext ctx mod
        resolvedParams <- forM p $ mapType $ follow ctx tctx
        let disc = if null $ tpNamespace discriminant
              then discriminant
              else subPath (monomorphName tp resolvedParams)
                           (tpName discriminant)
        resolvedArgs <- forMWithErrors args $ \(name, x) -> do
          x <- r x
          return (name, x)
        return $ IrEnumInit f disc resolvedArgs
      (EnumField x variantName fieldName) -> do
        r1 <- r x
        let (TypeInstance tp params) = inferredType x
        return
          $ IrField (IrField (IrField r1 variantFieldName) variantName)
          $ fieldName
      (TupleInit slots) -> do
        resolvedSlots <- forMWithErrors
          slots
          (\s -> do
            r1 <- r s
            return r1
          )
        return $ IrTupleInit f resolvedSlots
      (TupleSlot x slot) -> do
        r1 <- r x
        return $ IrField r1 (s_pack $ "__slot" ++ show slot)
      (Box i@(TraitImplementation { implTrait = TypeTraitConstraint ((modPath, traitName), params) }) e1)
        -> do
          r1     <- r e1
          for    <- findUnderlyingType ctx mod (Just pos) $ implFor i
          tctx   <- modTypeContext ctx mod
          params <- forMWithErrors params $ mapType (follow ctx tctx)
          let structName =
                subPath (monomorphName (modPath, traitName) params) "box"
          return $ IrStructInit
            (BasicTypeStruct structName)
            [ (valuePointerName, r1)
            , ( vtablePointerName
              , IrPreUnop
                Ref
                (IrIdentifier $ monomorphName
                  (monomorphName (modPath, traitName) params)
                  [implFor i]
                )
              )
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
        t <- findUnderlyingType ctx mod (Just pos) t
        return $ IrSizeOf t
      (Null ) -> return IrNull
      (Empty) -> do
        t' <- findUnderlyingType ctx mod (Just pos) t
        case t' of
          CArray              _ _ -> return ()
          BasicTypeAnonStruct _ _ -> return ()
          BasicTypeStruct      _  -> return ()
          BasicTypeComplexEnum _  -> return ()
          _                       -> throwk $ TypingError
            ("`empty` isn't a valid value of type " ++ show t')
            pos
        return IrEmpty
      t -> do
        throwk $ InternalError
          ("Unexpected expression in typed AST:\n\n" ++ show t)
          (Just pos)

{-addHeader :: Module -> FilePath -> Span -> IO ()
addHeader mod fp pos = do
  includes <- readIORef (modIncludes mod)
  unless (elem fp (map fst includes))
    $ modifyIORef (modIncludes mod) (\x -> (fp, pos) : x)-}
