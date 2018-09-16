module Kit.Compiler.Passes.TypeModuleContent where

import Control.Exception
import Control.Monad
import Data.Function
import Data.IORef
import Data.List
import Data.Maybe
import System.FilePath
import Kit.Ast
import Kit.Compiler.Binding
import Kit.Compiler.Context
import Kit.Compiler.DumpAst
import Kit.Compiler.Module
import Kit.Compiler.Passes.GenerateMonomorphs
import Kit.Compiler.Passes.SpecializeTypes
import Kit.Compiler.Scope
import Kit.Compiler.TypeContext
import Kit.Compiler.TypedDecl
import Kit.Compiler.TypedExpr
import Kit.Compiler.Typers
import Kit.Compiler.Unify
import Kit.Compiler.Utils
import Kit.Error
import Kit.HashTable
import Kit.Parser
import Kit.Str

{-
  Performs type resolution and type checking; converts Expr into TypedExpr and
  (Maybe TypeSpec) annotations into ConcreteTypes.

  See Kit.Compiler.Typers.* for specific typing implementations.
-}
typeContent
  :: CompileContext
  -> [(Module, [TypedDeclWithContext])]
  -> IO [(Module, TypedDecl)]
typeContent ctx modContent = do
  mapMWithErrors_ (typeModuleImplicits ctx) $ map fst modContent
  results <- typeIterative
    ctx
    [ (mod, decl) | (mod, decls) <- modContent, decl <- decls ]
    []
    (ctxRecursionLimit ctx)
  return results

typeModuleImplicits :: CompileContext -> Module -> IO ()
typeModuleImplicits ctx mod = do
  tctx  <- modTypeContext ctx mod
  using <- readIORef $ modUsing mod
  using <- forM using $ \u -> do
    case u of
      UsingImplicit x -> do
        x <- typeExpr ctx tctx mod x
        return $ UsingImplicit x
      x -> return x
  writeIORef (modUsing mod) using

data TypingStatus
  = Complete (Module, TypedDecl)
  | Incomplete Module TypedDeclWithContext (Maybe KitError)

mono params mono = (null params) == (null mono)

typeIterative
  :: CompileContext
  -> [(Module, TypedDeclWithContext)]
  -> [(Module, TypedDecl)]
  -> Int
  -> IO [(Module, TypedDecl)]
typeIterative ctx input output limit = do
  noisyDebugLog ctx
    $  "Beginning new typing pass; "
    ++ show limit
    ++ " remaining"
  results <- forM input $ \(mod, (d, tctx)) -> do
    let singleResult x = do
          x <- x
          return (Just (mod, x), [])
    result <-
      (try $ case d of
          -- if there are type parameters in the definition, this is a
          -- template and not a monomorph, so skip it for now;
          -- generateMonomorphs will add all realized monomorphs to the
          -- input stack without parameters
        DeclVar v -> singleResult $ typeVar ctx tctx mod v
        DeclFunction f | mono (functionParams f) (functionMonomorph f) ->
          singleResult $ typeFunction ctx tctx mod f
        DeclType t | mono (typeParams t) (typeMonomorph t) -> do
          DeclType x <- typeType ctx tctx mod t
          x          <- followType ctx tctx x
          -- split out methods/static fields into separate declarations
          let thisType     = TypeInstance (typeName x) (typeMonomorph t)
          let staticTctx   = tctx { tctxSelf = Just thisType }
          let instanceTctx = staticTctx { tctxThis = tctxSelf staticTctx }
          return
            ( Just
              ( mod
              , DeclType x { typeStaticFields  = []
                           , typeStaticMethods = []
                           , typeMethods       = []
                           }
              )
            , [ (DeclVar $ v { varBundle = Just $ typeRealName x }, staticTctx)
              | v <- typeStaticFields x
              ]
            ++ [ ( DeclFunction $ f
                   { functionName   = subPath (typeRealName t)
                     $ tpName
                     $ functionName f
                   , functionBundle = Just $ typeRealName x
                   }
                 , staticTctx
                 )
               | f <- typeStaticMethods x
               ]
            ++ [ ( DeclFunction $ f
                   { functionName   = subPath (typeRealName t)
                     $ tpName
                     $ functionName f
                   , functionBundle = Just $ typeRealName x
                   }
                 , instanceTctx
                 )
               | f <- typeMethods x
               ]
            )
        DeclTrait t | mono (traitAllParams t) (traitMonomorph t) -> do
          DeclTrait x <- typeTrait ctx tctx mod t
          return (Just (mod, DeclTrait $ x), [])
        DeclImpl i -> do
          let thisType = implFor i
          let pos      = implPos i
          let tctx' =
                tctx { tctxSelf = Just $ thisType, tctxThis = Just $ thisType }
          DeclImpl x <- typeImpl ctx tctx' mod i
          return (Just (mod, DeclImpl $ x), [])
        DeclRuleSet rs -> return (Nothing, [])
        _              -> return (Nothing, [])
      ) :: IO
        (Either KitError (Maybe (Module, TypedDecl), [TypedDeclWithContext]))
    case result of
      Left  e            -> do
        noisyDebugLog ctx $ "typing failed"
        return [Incomplete mod (d, tctx) (Just e)]
      Right (Nothing, x) -> return [ Incomplete mod xi Nothing | xi <- x ]
      Right (Just d, x) ->
        return $ (Complete d) : [ Incomplete mod xi Nothing | xi <- x ]

  let collapsedResults = foldr (++) [] results

  let incomplete = catMaybes
        [ case result of
            Complete _         -> Nothing
            Incomplete mod d _ -> Just (mod, d)
        | result <- collapsedResults
        ]
  let complete =
        output
          ++ (catMaybes
               [ case result of
                   Complete d -> Just d
                   _          -> Nothing
               | result <- collapsedResults
               ]
             )

  let errors =
        (foldr
          (\x acc -> case x of
            Incomplete _ _ (Just e) -> e : acc
            _                       -> acc
          )
          []
          (reverse collapsedResults)
        )

  let decls = map snd

  when (limit <= 0)
    $ throwk
    $ KitErrors
    $ (KitError $ BasicError
        ("Maximum number of compile passes exceeded while typing")
        Nothing
      )
    : (reverse errors)

  if (map fst (decls incomplete) == map fst (decls input)) || (null incomplete)
    then do
      newMonomorphs <- generateMonomorphs ctx
      specialized   <- specializeTypes ctx
      if (not $ null newMonomorphs) || specialized
        then typeIterative ctx
                           (incomplete ++ newMonomorphs)
                           complete
                           (limit - 1)
        else if null incomplete
          then return complete
          else
            throwk
            $ KitErrors
            $ (KitError $ BasicError
                "Failed typing; halting due to the following unsolvable errors:"
                Nothing
              )
            : (reverse errors)
    else typeIterative ctx incomplete complete (limit - 1)
