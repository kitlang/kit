module Kit.Compiler.TermRewrite where

import Control.Monad
import Data.List
import Kit.Ast
import Kit.Compiler.Context
import Kit.Compiler.Module
import Kit.Compiler.TypeContext
import Kit.Compiler.Unify
import Kit.Compiler.Utils
import Kit.Error
import Kit.Log
import Kit.Ast.Span
import Kit.Str

data TermRewriteError
  = TermRewriteError String [(String, RewriteRule TypedExpr ConcreteType)] Span
  deriving (Eq, Show)
instance Errable TermRewriteError where
  logError e@(TermRewriteError msg rules pos) = do
    logErrorBasic e $ msg
    forM_ rules (\(msg, rule) -> do ePutStrLn msg; displayFileSnippet (rulePos rule))

  errPos (TermRewriteError _ _ pos) = Just pos

type RuleBinding = (Str, TypedExpr)

rewriteExpr
  :: CompileContext
  -> TypeContext
  -> Module
  -> RewriteRule TypedExpr ConcreteType
  -> TypedExpr
  -> (TypeContext -> TypedExpr -> IO TypedExpr)
  -> IO (Maybe TypedExpr)
rewriteExpr ctx tctx mod rule te typer = do
  match <- ruleMatch ctx tctx (rulePattern rule) te (ruleThis rule)
  case (match, ruleBody rule) of
    (Just x, Just body) -> do
      noisyDebugLog ctx $ "rule match at " ++ show (tPos te)
      tctx <- return $ tctx
        { tctxActiveRules = (rule, tPos te) : tctxActiveRules tctx
        , tctxMacroVars   = x ++ tctxMacroVars tctx
        }
      when ((length $ tctxActiveRules tctx) > ctxRecursionLimit ctx)
        $ let [firstRule, prevRule] = take 2 $ reverse $ tctxActiveRules tctx
          in
            do
              throwk $ TermRewriteError
                "Maximum number of rewrite rule applications exceeded, starting here:"
                (nubBy
                  (\(_, a) (_, b) -> a == b)
                  [ ("        Last matching rule:  ", rule)
                  , ("    Previous matching rule:  ", fst prevRule)
                  , ("       First matching rule:  ", fst firstRule)
                  ]
                )
                (snd firstRule)
      t <- typer tctx body
      return $ Just $ t { rewrittenBy = Just rule, tPos = tPos te }
    _ -> return Nothing

{-
  Checks whether a typed expression matches a given pattern.

  Returns Nothing on no match, or Just with a list of macro variable to
  expression bindings.

  The expression discriminant and children will be compared automatically; if
  an expression type contains anything else that should be compared, it needs
  to be checked explicitly here.
-}
-- TODO: very incomplete
ruleMatch
  :: CompileContext
  -> TypeContext
  -> TypedExpr
  -> TypedExpr
  -> Maybe TypedExpr
  -> IO (Maybe [RuleBinding])
ruleMatch ctx tctx pattern te thisExpr = do
  let combineResults = foldM
        (\acc x -> case (acc, x) of
          (Just y, Just x) -> return $ Just (y ++ x)
          _                -> return Nothing
        )
        (Just [])
  let r x y = ruleMatch ctx tctx x y thisExpr
  case (tExpr pattern, tExpr te) of
    (Identifier (MacroVar "this" _), y) -> do
      case thisExpr of
        Just thisExpr -> do
          match <- unifyStrict ctx
                               tctx
                               (inferredType thisExpr)
                               (inferredType te)
          case match of
            Just _  -> return $ Just [("this", te)]
            Nothing -> return Nothing
        Nothing -> return Nothing
    (Identifier (MacroVar x t), y) -> do
      -- ${var: type} - match and bind only if the type matches
      if (t == TypeBasicType BasicTypeUnknown) || (t == inferredType te)
        then return $ Just [(x, te)]
        else return Nothing
    (Identifier (Var x), Identifier (Var y)) ->
      return $ if x == y then Just [] else Nothing
    (Identifier _, Identifier _) -> return Nothing
    (Literal a _, Literal b _) ->
      return $ if valueEq a b then Just [] else Nothing
    (Binop op1 a b, Binop op2 c d) -> if op1 == op2
      then do
        x <- r a c
        y <- r b d
        combineResults [x, y]
      else return Nothing
    (PreUnop op1 x, PreUnop op2 y) ->
      if op1 == op2 then r x y else return Nothing
    (PostUnop op1 x, PostUnop op2 y) ->
      if op1 == op2 then r x y else return Nothing
    (Field x (Var a), Field y (Var b)) ->
      if a == b then r x y else return Nothing
    (Cast x t1, Cast y t2) -> if t1 == t2 then r x y else return Nothing
    (ArrayLiteral x, ArrayLiteral y) -> if length x == length y
      then do
        results <- forM (zip x y) $ \(a, b) -> r a b
        combineResults results
      else return Nothing
    (RangeLiteral a b, RangeLiteral c d) -> do
      x <- r a c
      y <- r b d
      combineResults [x, y]
    (a, b) -> if exprDiscriminant a == exprDiscriminant b
      then
        let (c1, c2) = (exprChildren a, exprChildren b)
        in  if length c1 == length c2
              then do
                children <- mapM
                  (\(pattern, te) -> ruleMatch ctx tctx pattern te thisExpr)
                  (zip c1 c2)
                combineResults children
              else return Nothing
      else return Nothing
