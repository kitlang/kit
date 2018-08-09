module Kit.Compiler.TermRewrite where

import Control.Monad
import Data.List
import Kit.Ast
import Kit.Compiler.Context
import Kit.Compiler.Module
import Kit.Compiler.TypeContext
import Kit.Compiler.TypedExpr
import Kit.Error
import Kit.HashTable
import Kit.Log
import Kit.Parser.Span
import Kit.Str

data TermRewriteError = TermRewriteError String [(String, RewriteRule Expr (Maybe TypeSpec))] Span deriving (Eq, Show)
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
  -> RewriteRule Expr (Maybe TypeSpec)
  -> TypedExpr
  -> (TypeContext -> Expr -> IO TypedExpr)
  -> IO (Maybe TypedExpr)
rewriteExpr ctx tctx mod rule te typer = do
  match <- ruleMatch (rulePattern rule)
                     te
                     (tctxThis tctx)
                     (resolveType ctx tctx mod)
  case (match, ruleBody rule) of
    (Just x, Just body) -> do
      let tctx' = tctx
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
                  [ ("Last matching rule:"    , rule)
                  , ("Previous matching rule:", fst prevRule)
                  , ("First matching rule:"   , fst firstRule)
                  ]
                )
                (snd firstRule)
      t <- typer tctx' (subst x body)
      return $ Just $ t {rewrittenBy = Just rule, tPos = (tPos t) {rewrittenFrom = Just $ tPos te}}
    _ -> return Nothing

-- TODO
subst :: [RuleBinding] -> Expr -> Expr
subst r x = x

{-
  Checks whether a typed expression matches a given pattern.

  Returns Nothing on no match, or Just with a list of macro variable to
  expression bindings.
-}
-- TODO: very incomplete
ruleMatch
  :: Expr
  -> TypedExpr
  -> Maybe ConcreteType
  -> (TypeSpec -> IO ConcreteType)
  -> IO (Maybe [RuleBinding])
ruleMatch pattern te thisType typeResolver = do
  let combineResults = foldM
        (\acc x -> case (acc, x) of
          (Just y, Just x) -> return $ Just (y ++ x)
          _                -> return Nothing
        )
        (Just [])
  case (expr pattern, texpr te) of
    (Identifier (MacroVar x (Just t)) [], y) -> do
      -- ${var: type} - match and bind only if the type matches
      macroVarType <- typeResolver t
      if macroVarType == inferredType te
        then return $ Just [(x, te)]
        else return Nothing
    (Identifier (MacroVar "this" Nothing) [], y) -> case thisType of
      -- $this - match and bind if type matches "this" in context
      Just thisType -> if thisType == inferredType te
        then return $ Just [("this", te)]
        else return Nothing
      Nothing -> return $ Just [("this", te)]
    (Identifier (MacroVar x Nothing) [], y) ->
      -- $var - match and bind anything
      return $ Just [(x, te)]
    (Identifier (Var x) n, Identifier (Var y) m) ->
      return $ if (x, n) == (y, m) then Just [] else Nothing
    (Identifier _ _, Identifier _ _) -> return Nothing
    (Literal a, Literal b) -> return $ if valueEq a b then Just [] else Nothing
    (a        , b        ) -> if exprDiscriminant a == exprDiscriminant b
      then
        let (c1, c2) = (exprChildren a, exprChildren b)
        in
          if length c1 == length c2
            then do
              children <- mapM
                (\(pattern, te) -> ruleMatch pattern te thisType typeResolver)
                (zip c1 c2)
              combineResults children
            else return Nothing
      else return Nothing
