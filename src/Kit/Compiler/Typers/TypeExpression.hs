module Kit.Compiler.Typers.TypeExpression where

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
import Kit.Compiler.TermRewrite
import Kit.Compiler.TypeContext
import Kit.Compiler.Typers.AutoRefDeref
import Kit.Compiler.Typers.ExprTyper
import Kit.Compiler.Typers.TypeExpression.TypeArrayAccess
import Kit.Compiler.Typers.TypeExpression.TypeCall
import Kit.Compiler.Typers.TypeExpression.TypeCast
import Kit.Compiler.Typers.TypeExpression.TypeControl
import Kit.Compiler.Typers.TypeExpression.TypeField
import Kit.Compiler.Typers.TypeExpression.TypeIdentifier
import Kit.Compiler.Typers.TypeExpression.TypeLiteral
import Kit.Compiler.Typers.TypeExpression.TypeMatch
import Kit.Compiler.Typers.TypeExpression.TypeOp
import Kit.Compiler.Typers.TypeExpression.TypeStructInit
import Kit.Compiler.Typers.TypeExpression.TypeVarBinding
import Kit.Compiler.Typers.TypeExpression.TypeVarDeclaration
import Kit.Compiler.Unify
import Kit.Compiler.Utils
import Kit.Error
import Kit.Parser
import Kit.Str

typeMaybeExpr
  :: CompileContext
  -> TypeContext
  -> Module
  -> Maybe TypedExpr
  -> IO (Maybe TypedExpr)
typeMaybeExpr ctx tctx mod e = case e of
  Just ex -> do
    result <- typeExpr ctx tctx mod ex
    return $ Just result
  Nothing -> return Nothing

{-
  Converts a tree of untyped AST to typed AST.

  The compile process is roughly:

    untyped AST (Expr)
    -> typed AST (TypedExpr) <- we are generating this
    -> IR (IrExpr)
    -> generated code

  After typing a subexpression, rewrite rules may take effect and trigger
  an early exit.
-}
typeExpr :: CompileContext -> TypeContext -> Module -> TypedExpr -> IO TypedExpr
typeExpr ctx tctx mod ex@(TypedExpr { tExpr = et, tPos = pos }) = do
  let r = typeExpr ctx tctx mod
  let maybeR x = case x of
        Just x -> do
          x' <- r x
          return $ Just x'
        Nothing -> return Nothing
  let resolve constraint = resolveConstraint ctx tctx constraint
  let
    tryRewrite x y = do
      ownRules <- ownRules ctx tctx x
      let implicitRules =
            nub
              $  ownRules
              ++ (foldr (++) [] $ map tImplicitRules $ exprChildren $ tExpr x)
      let
        rulesByType = groupBy
          (\x y -> ruleThis x == ruleThis y)
          (  implicitRules
          ++ [ rule
             | ruleset <- tctxRules tctx
             , rule    <- ruleSetRules ruleset
             ]
          )
      result <- foldM
        (\acc rules -> do
          case acc of
            Just x  -> return $ Just x
            Nothing -> do
              gtctx <- case ruleThis $ head rules of
                Just x  -> genericTctx ctx tctx (tPos x) (inferredType x)
                Nothing -> return tctx
              foldM
                (\acc rule -> do
                  case acc of
                    Just x  -> return $ Just x
                    Nothing -> rewriteExpr ctx
                                           gtctx
                                           mod
                                           rule
                                           x
                                           (\tctx -> typeExpr ctx tctx mod)
                )
                Nothing
                rules
        )
        Nothing
        rulesByType
      case result of
        Just x  -> r x
        Nothing -> y

  let utils = TyperUtils
        { _r          = r
        , _maybeR     = maybeR
        , _tryRewrite = tryRewrite
        , _resolve    = resolve
        , _typeExpr   = typeExpr
        }

  let subTyper f = f utils ctx tctx mod ex

  result <- case et of
    (Block children) -> do
      blockScope <- newScope
      tctx <- return $ tctx { tctxScopes = blockScope : tctxScopes tctx }
      typedChildren <- forMWithErrors children $ typeExpr ctx tctx mod
      return $ makeExprTyped (Block typedChildren) (TypeVoid) pos

    (Using using e1) -> do
      tctx' <- foldM
        (\c use -> do
          using' <- case use of
            UsingImplicit x ->
              (do
                x' <- r x
                return $ UsingImplicit x'
              )
            _ -> return use
          addUsing ctx c using'
        )
        tctx
        using
      r1 <- typeExpr ctx tctx' mod e1
      return $ makeExprTyped (Using using r1) (inferredType $ r1) pos

    (Meta m e1) -> do
      r1 <- r e1
      return $ makeExprTyped (Meta m r1) (inferredType r1) pos

    (Literal l _) -> do
      mapM_ resolve $ literalConstraints l (inferredType ex) pos
      return $ ex { tCompileTimeValue = Just l }

    (This) -> do
      case tctxThis tctx of
        Just t -> return $ (makeExprTyped This t pos) { tIsLvalue = True }
        Nothing ->
          throwk $ TypingError ("`this` can only be used in methods") pos

    (Self) -> do
      case tctxSelf tctx of
        Just (TypeInstance tp params) ->
          return $ makeExprTyped Self (TypeTypeOf tp params) pos
        Just x -> throwk $ TypingError
          ("`Self` value of " ++ show x ++ " not yet supported")
          pos
        Nothing ->
          throwk $ TypingError ("`Self` can only be used in methods") pos

    (Identifier _       ) -> subTyper typeIdentifier

    (TypeAnnotation e1 t) -> do
      r1 <- r e1
      let e = r1 { inferredType = t }
      tryRewrite e $ do
        resolve $ TypeEq
          t
          (inferredType r1)
          "Annotated expressions must match their type annotation"
          (tPos r1)
        return e

    (PreUnop  _ _      ) -> subTyper typeOp
    (PostUnop _ _      ) -> subTyper typeOp
    (Binop _ _ _       ) -> subTyper typeOp

    (For   _ _ _       ) -> subTyper typeControl
    (While _ _ _       ) -> subTyper typeControl
    (If    _ _ _       ) -> subTyper typeControl
    (Continue          ) -> subTyper typeControl
    (Break             ) -> subTyper typeControl

    (Call e1 _ args    ) -> subTyper typeCall
    (Return _          ) -> subTyper typeCall

    (Match _ _ _       ) -> subTyper typeMatch

    (InlineCall e1) -> throwk $ InternalError "Not yet implemented" (Just pos)

    (Field        _  _ ) -> subTyper typeField

    (ArrayAccess  _  _ ) -> subTyper typeArrayAccess

    (Cast         _  _ ) -> subTyper typeCast

    (RangeLiteral e1 e2) -> do
      r1 <- r e1
      r2 <- r e2
      resolve $ TypeEq typeClassIntegral
                       (inferredType r1)
                       "Range literal arguments must be integer types"
                       (tPos r1)
      resolve $ TypeEq typeClassIntegral
                       (inferredType r2)
                       "Range literal arguments must be integer types"
                       (tPos r2)
      return $ makeExprTyped (RangeLiteral r1 r2) TypeRange pos

    (ArrayLiteral items) -> do
      items <- mapM r items
      let handle t s = when (s > 0) $ do
            unless (length items == s) $ throwk $ TypingError
              (  "The Array's length parameter ("
              ++ show s
              ++ ") must match the number of elements ("
              ++ show (length items)
              ++ ")"
              )
              pos
            forM_ items $ \val -> resolve $ TypeEq
              t
              (inferredType val)
              "Array elements must match the array's value type"
              (tPos val)
      case inferredType ex of
        TypeArray t s             -> handle t s
        TypeConst (TypeArray t s) -> handle t s
        _                         -> throwk $ TypingError
          (  "Array literals must be typed as arrays; this array typed as "
          ++ show (inferredType ex)
          )
          pos
      return $ makeExprTyped (ArrayLiteral items) (inferredType ex) pos

    (LocalVarDeclaration _ _ _ _) -> subTyper typeVarDeclaration

    (StructInit _ _             ) -> subTyper typeStructInit
    (UnionInit  _ _             ) -> subTyper typeStructInit

    (TupleInit slots            ) -> do
      slots' <- forMWithErrors slots r
      return $ makeExprTyped (TupleInit slots')
                             (TypeTuple (map inferredType slots'))
                             pos

    (Implicit t) -> do
      t   <- follow ctx tctx t
      val <- findImplicit ctx tctx t (tctxImplicits tctx)
      case val of
        Just (_, x) -> return x
        Nothing     -> throwk $ TypingError
          (  "Couldn't find an implicit value of type "
          ++ show t
          ++ ".\n\nImplicits in scope:\n\n"
          ++ intercalate
               "\n"
               [ "  - implicit " ++ (show $ inferredType i)
               | i <- tctxImplicits tctx
               ]
          )
          pos

    (Temp x) -> do
      rx <- r x
      return $ (makeExprTyped (Temp rx) (inferredType rx) (tPos rx)) { tIsLocal = True
                                                                     }

    (SizeOf t) -> do
      t <- follow ctx tctx $ t
      return $ makeExprTyped (SizeOf t) (inferredType ex) pos

    (StaticExpr x@(TypedExpr { tExpr = If cond then_ else_ })) -> do
      cond <- r cond
      let val = tCompileTimeValue cond
      case val of
        Just (BoolValue True) -> do
          r then_
        Just (BoolValue False) -> case else_ of
          Just x -> r x
          _      -> r $ makeExprTyped (Block []) TypeVoid pos
        Just v -> throwk $ TypingError
          ("`static if` condition evaluated to a non-Bool value: " ++ show v)
          (tPos cond)
        Nothing -> throwk $ TypingError
          ("`static if` condition couldn't be evaluated at compile time")
          (tPos cond)

    (StaticExpr x) -> do
      x <- r x
      let val = tCompileTimeValue x
      case val of
        Just value -> r $ makeExprTyped (Literal value (inferredType ex))
                                        (inferredType ex)
                                        pos
        Nothing -> throwk $ TypingError
          ("static expression couldn't be evaluated at compile time")
          (tPos x)

    _ -> return $ ex

  t'       <- follow ctx tctx $ inferredType result
  result   <- return $ result { inferredType = t' }
  ownRules <- ownRules ctx tctx result
  result   <- return $ result { tImplicitRules = ownRules }
  tryRewrite result $ return result

ownRules ctx tctx this = case inferredType this of
  TypeBox tp params -> do
    def <- getTraitDefinition ctx tp
    return [ rule { ruleThis = Just this } | rule <- traitRules def ]
  TypeInstance tp params -> do
    def <- getTypeDefinition ctx tp
    let rules = [ rule { ruleThis = Just this } | rule <- typeRules def ]
    case typeSubtype def of
      Abstract { abstractUnderlyingType = parent@(TypeInstance tp params) } ->
        do
          parent      <- follow ctx tctx parent
          parentRules <- ownRules ctx tctx $ this { inferredType = parent }
          return
            $  rules
            ++ [ rule { ruleThis = Just this } | rule <- parentRules ]
      _ -> return rules
  _ -> return []
