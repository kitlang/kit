module Kit.Compiler.Passes.TypeModuleContent (typeContent) where

import Control.Exception
import Control.Monad
import Data.Function
import Data.Mutable
import Data.List
import Data.Maybe
import Kit.Ast
import Kit.Compiler.Context
import Kit.Compiler.Module
import Kit.Compiler.Passes.GenerateMonomorphs
import Kit.Compiler.Passes.SpecializeTypes
import Kit.Compiler.TypeContext
import Kit.Compiler.TypedStmt
import Kit.Compiler.Typers
import Kit.Compiler.Utils
import Kit.Error

{-
  Performs type resolution and type checking; converts Expr into TypedExpr and
  (Maybe TypeSpec) annotations into ConcreteTypes.

  See Kit.Compiler.Typers.* for specific typing implementations.
-}
typeContent
  :: CompileContext
  -> [(Module, [TypedStmtWithContext])]
  -> IO [(Module, TypedStmt)]
typeContent ctx modContent = do
  mapMWithErrors_ (typeModuleImplicits ctx) $ map fst modContent
  results <- typeIterative
    ctx
    [ (mod, decl) | (mod, decls) <- modContent, decl <- decls ]
    []
    []
    (ctxRecursionLimit ctx)
  return results

typeModuleImplicits :: CompileContext -> Module -> IO ()
typeModuleImplicits ctx mod = do
  tctx  <- modTypeContext ctx mod
  using <- readRef $ modUsing mod
  using <- forM using $ \u -> do
    case u of
      UsingImplicit x -> do
        x <- typeExpr ctx tctx mod x
        return $ UsingImplicit x
      x -> return x
  writeRef (modUsing mod) using

data TypingStatus
  = Complete (Module, TypedStmt)
  | Incomplete Module TypedStmtWithContext (Maybe KitError)

mono params mono = (null params) == (null mono)

typeIterative
  :: CompileContext
  -> [(Module, TypedStmtWithContext)]
  -> [(Module, TypedStmt)]
  -> [KitError]
  -> Int
  -> IO [(Module, TypedStmt)]
typeIterative ctx input output lastErrors limit = do
  noisyDebugLog ctx
    $  "Beginning new typing pass; "
    ++ show limit
    ++ " remaining"
  results <- forMWithErrors input $ \(mod, (d, tctx)) -> do
    let singleResult x = do
          x <- x
          return (Just (mod, x), [])
    result <-
      (try $ case stmt d of
          -- if there are type parameters in the definition, this is a
          -- template and not a monomorph, so skip it for now;
          -- generateMonomorphs will add all realized monomorphs to the
          -- input stack without parameters
        VarDeclaration v -> singleResult $ typeVar ctx tctx mod v
        FunctionDeclaration f | mono (functionParams f) (functionMonomorph f) ->
          singleResult $ typeFunction ctx tctx mod f
        TypeDeclaration t | mono (typeParams t) (typeMonomorph t) -> do
          Statement { stmt = TypeDeclaration x } <- typeType ctx tctx mod t
          x <- followType ctx tctx x
          -- split out methods/static fields into separate declarations
          let thisType     = TypeInstance (typeName x) (typeMonomorph t)
          let staticTctx   = tctx { tctxSelf = Just thisType }
          let instanceTctx = staticTctx { tctxThis = tctxSelf staticTctx }
          return
            ( if hasMeta "builtin" (typeMeta t)
              then Nothing
              else Just
                ( mod
                , ps (typePos x) $ TypeDeclaration $ x { typeStaticFields  = []
                                                       , typeStaticMethods = []
                                                       , typeMethods       = []
                                                       }
                )
            , [ ( ps (varPos v) $ VarDeclaration $ v
                  { varName   = subPath (typeRealName t) $ tpName $ varName v
                  , varBundle = Just $ typeName x
                  }
                , staticTctx
                )
              | v <- typeStaticFields x
              ]
            ++ [ ( ps (functionPos f) $ FunctionDeclaration $ f
                   { functionName   = subPath (typeRealName t)
                     $ tpName
                     $ functionName f
                   , functionBundle = Just $ typeName x
                   }
                 , staticTctx
                 )
               | f <- typeStaticMethods x
               ]
            ++ [ ( ps (functionPos f) $ FunctionDeclaration $ f
                   { functionName   = subPath (typeRealName t)
                     $ tpName
                     $ functionName f
                   , functionBundle = Just $ typeName x
                   }
                 , instanceTctx
                 )
               | f <- typeMethods x
               ]
            )
        TraitDeclaration t | mono (traitAllParams t) (traitMonomorph t) -> do
          Statement { stmt = TraitDeclaration x } <- typeTrait ctx tctx mod t
          return (Just (mod, ps (traitPos t) $ TraitDeclaration x), [])
        Implement i -> do
          let thisType = implFor i
          let pos      = implPos i
          let tctx' =
                tctx { tctxSelf = Just $ thisType, tctxThis = Just $ thisType }
          Statement { stmt = Implement x } <- typeImpl ctx tctx' mod i
          return (Just (mod, ps (implPos x) $ Implement x), [])
        RuleSetDeclaration rs -> return (Nothing, [])
        _                     -> return (Nothing, [])
      ) :: IO
        (Either KitError (Maybe (Module, TypedStmt), [TypedStmtWithContext]))
    case result of
      Left  e            -> return [Incomplete mod (d, tctx) (Just e)]
      Right (Nothing, x) -> do
        markProgress ctx "completed typing"
        return [ Incomplete mod xi Nothing | xi <- x ]
      Right (Just d, x) -> do
        markProgress ctx "completed typing"
        return $ (Complete d) : [ Incomplete mod xi Nothing | xi <- x ]

  let collapsedResults               = foldr (++) [] results
  let (incomplete, complete, errors) = splitComplete collapsedResults [] [] []

  when (limit <= 0)
    $ throwk
    $ KitErrors
    $ (KitError $ BasicError
        ("Maximum number of compile passes exceeded while typing")
        Nothing
      )
    : (reverse errors)

  madeProgress <- readRef (ctxMadeProgress ctx)
  writeRef (ctxMadeProgress ctx) False
  if (null incomplete) || madeProgress || (madeProgress || errors /= lastErrors)
    then do
      newMonomorphs <- generateMonomorphs ctx
      specialized   <- specializeTypes ctx
      if (not $ null newMonomorphs) || specialized
        then typeIterative ctx
                           (incomplete ++ newMonomorphs)
                           (complete ++ output)
                           errors
                           (limit - 1)
        else if null incomplete
          then return $ complete ++ output
          else throwk $ KitErrors $ reverse errors
    else throwk $ KitErrors $ reverse errors

splitComplete [] incomplete complete errors = (incomplete, complete, errors)
splitComplete (h : t) incomplete complete errors = case h of
  Complete d -> splitComplete t incomplete (d : complete) errors
  Incomplete mod d (Just e) ->
    splitComplete t ((mod, d) : incomplete) complete (e : errors)
  Incomplete mod d _ -> splitComplete t ((mod, d) : incomplete) complete errors
