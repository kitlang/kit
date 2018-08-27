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
  :: CompileContext -> [(Module, [TypedDecl])] -> IO [(Module, [TypedDecl])]
typeContent ctx modContent = do
  results <- typeIterative ctx modContent [] (ctxRecursionLimit ctx)
  return results

data TypingStatus
  = Complete (Maybe TypedDecl)
  | Incomplete TypedDecl (Maybe KitError)

typeIterative
  :: CompileContext
  -> [(Module, [TypedDecl])]
  -> [(Module, [TypedDecl])]
  -> Int
  -> IO [(Module, [TypedDecl])]
typeIterative ctx input output limit = do
  results <- forM
    input
    (\(mod, decls) -> do
      results <- forM
        decls
        (\d -> do
          result <-
            (try $ case d of
              DeclVar      v -> typeVar ctx mod v
              DeclFunction f | null (functionParams f) -> typeFunction ctx mod f
              DeclType     t | null (typeParams t) -> typeType ctx mod t
              DeclTrait    t -> typeTrait ctx mod t
              DeclImpl     i -> typeImpl ctx mod i
              DeclRuleSet  rs -> return (Nothing, True)
              _ -> return (Nothing, True)
            ) :: IO (Either KitError (Maybe TypedDecl, Bool))
          case result of
            Left  e                 -> return $ Incomplete d (Just e)
            Right (d       , True ) -> return $ Complete d
            Right ((Just x), False) -> return $ Incomplete x Nothing
        )
      return (mod, results)
    )

  let incompletes x = foldr
        (\x acc -> case x of
          Incomplete d _ -> d : acc
          _              -> acc
        )
        []
        x
  let completes x = foldr
        (\x acc -> case x of
          Complete (Just d) -> d : acc
          _                 -> acc
        )
        []
        x

  let incomplete =
        [ (mod, incompletes r)
        | (mod, r) <- results
        , not $ null $ incompletes r
        ]
  let complete =
        output
          ++ [ (mod, completes r)
             | (mod, r) <- results
             ]

  let errors =
        (foldr
          (\x acc -> case x of
            Incomplete _ (Just e) -> e : acc
            _                     -> acc
          )
          []
          (reverse $ foldr (++) [] (map snd results))
        )

  let decls x = foldr (++) [] (map snd x)
  when (decls incomplete == decls input)
    $ throwk
    $ KitErrors
    $ (KitError $ BasicError
        "Failed typing; halting due to the following unsolvable errors:"
        Nothing
      )
    : errors

  when (limit <= 0) $ if ctxDumpAst ctx
    then do
      throwk
        $ KitErrors
        $ (KitError $ BasicError
            ("Maximum number of compile passes exceeded while typing; incomplete content:"
            )
            Nothing
          )
        : errors
      mods <- ctxSourceModules ctx
      forM_
        input
        (\(mod, decls) -> do
          putStrLn $ show mod
          forM_ decls (dumpModuleDecl ctx mod 0)
          putStrLn ""
        )
    else
      throwk
      $ KitErrors
      $ (KitError $ BasicError
          (  "Maximum number of compile passes exceeded while typing;"
          ++ "; run again with --dump-ast to see typed output"
          )
          Nothing
        )
      : errors

  if null incomplete
    then return complete
    else typeIterative ctx incomplete complete (limit - 1)
