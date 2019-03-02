module Kit.Compiler (
  decideModuleAndSourcePaths,
  tryCompile,
  module Kit.Compiler.Binding,
  module Kit.Compiler.Context,
  module Kit.Compiler.Module,
  module Kit.Compiler.Passes,
  module Kit.Compiler.Scope,
  module Kit.Compiler.TypeContext,
  module Kit.Compiler.Unify,
  module Kit.Compiler.Utils
) where

import Control.Exception
import Control.Monad
import Data.List
import Data.Time
import System.FilePath
import System.Process
import Kit.Ast
import Kit.Ir
import Kit.Compiler.Binding
import Kit.Compiler.Context
import Kit.Compiler.DumpAst
import Kit.Compiler.Module
import Kit.Compiler.Passes
import Kit.Compiler.Scope
import Kit.Compiler.TypeContext
import Kit.Compiler.Unify
import Kit.Compiler.Utils
import Kit.Error
import Kit.Log
import Kit.Str
import Kit.Toolchain

tryCompile :: CompileContext -> Toolchain -> Toolchain -> IO (Either KitError ())
tryCompile context build host = try $ compile context build host

{-
  Run compilation to completion from the given CompileContext. Throws an
  Error on failure.
-}
compile :: CompileContext -> Toolchain -> Toolchain -> IO ()
compile ctx build host = do
  startTime <- getCurrentTime

  {-
    Load the main module and all of its dependencies recursively. Also builds
    module interfaces, which declare the set of types that exist in a module
    and map them to type variables.
  -}
  printLogIf ctx "parsing and building module graph"
  declarations <- {-# SCC "compile_pass.build_module_graph" #-} buildModuleGraph ctx

  {-
    Generate C modules for all includes found during buildModuleGraph.
  -}
  printLogIf      ctx "processing C includes"
  {-# SCC "compile_pass.include_C_modules" #-} includeCModules ctx host

  {-
    Find and execute statement-level macros.
  -}
  printLogIf      ctx "expanding macros"
  declarations <- {-# SCC "compile_pass.expand_macros" #-} expandMacros ctx build compile declarations

  {-
    This step utilizes the module interfaces from buildModuleGraph to convert
    syntactic types to preliminary typed AST. Type annotations will be looked
    up and will fail if they don't resolve, but program semantics won't be
    checked yet; we'll get typed AST with a lot of spurious type variables,
    which will be unified later.
  -}
  printLogIf ctx "resolving module types"
  resolved <- {-# SCC "compile_pass.resolve_module_types" #-} resolveModuleTypes ctx declarations

  printLogIf ctx "flattening trait implementations"
  resolved <- {-# SCC "compile_pass.flatten_traits" #-} flattenTraits ctx resolved

  compilerSanityChecks ctx

  {-
    TODO: expand procedural macros here
  -}

  {-
    Main checking of program semantics happens here. Takes and returns typed
    AST, but the return value should have all necessary type information. This
    step is iterative and repeats until successful convergence, or throws an
    exception on failure.
  -}
  printLogIf ctx "typing module content"
  typedRaw <- {-# SCC "compile_pass.type_module_content" #-} typeContent ctx resolved
  let
    typed =
      [ (fst $ head x, map snd x)
      | x <- groupBy
        (\a b -> (modPath $ fst a) == (modPath $ fst b))
        (sortBy (\a b -> compare (modPath $ fst a) (modPath $ fst b)) typedRaw)
      , not $ null x
      ]

  when (ctxDumpAst ctx) $ do
    printLogIf ctx   "typed AST:"
    forM_      typed (\(mod, decls) -> dumpModuleContent ctx mod decls)

  {-
    Convert typed AST to IR.
  -}
  printLogIf ctx "generating intermediate representation"
  irRaw <- {-# SCC "compile_pass.generate_IR" #-} generateIr ctx typed
  let ir =
        [ (fst $ head x, [foldr mergeBundles (snd $ head x) (map snd $ tail x)])
        | x <- groupBy
          (\a b ->
            ((modPath $ fst a) == (modPath $ fst b))
              && (bundleTp (snd a) == bundleTp (snd b))
          )
          (sortBy
            (\a b -> compare (modPath $ fst a, bundleTp $ snd a)
                             (modPath $ fst b, bundleTp $ snd b)
            )
            [ (mod, decl) | (mod, decls) <- irRaw, decl <- decls ]
          )
        ]

  {-
    Generate header and code files from IR.
  -}
  printLogIf ctx "generating code"
  generated <- {-# SCC "compile_pass.generate_code" #-} generateCode ctx ir

  {-
    Compile the generated code.
  -}
  binPath   <- if ctxNoCompile ctx
    then do
      printLogIf ctx "skipping compile"
      return Nothing
    else do
      printLogIf ctx "compiling"
      compileCode ctx host generated

  endTime <- getCurrentTime
  printLogIf ctx
    $  "finished; total time: "
    ++ (show $ diffUTCTime endTime startTime)

  when (ctxRun ctx) $ case binPath of
    Just x -> do
      printLogIf ctx "running"
      case ctxResultHandler ctx of
        Just resultHandler -> do
          result <- readProcess x [] ""
          resultHandler result
        Nothing -> do
          callProcess x []
          return ()
    Nothing -> logMsg
      Nothing
      "--run was set, but no binary path was generated; skipping"

compilerSanityChecks :: CompileContext -> IO ()
compilerSanityChecks ctx = do
  let requiredTraits =
        [ typeClassNumericPath
        , typeClassIntegralPath
        , typeClassNumericMixedPath
        , typeClassIteratorPath
        , typeClassIterablePath
        , typeClassNumericPath
        ]
  forMWithErrors_ requiredTraits $ \t -> do
    result <-
      (try $ getTraitDefinition ctx t) :: IO
        (Either KitError (TraitDefinition TypedExpr ConcreteType))
    case result of
      Left _ -> throwk $ InternalError
        ("Sanity check failed: couldn't find required trait "
        ++ s_unpack (showTypePath t)
        ++ ", which should be provided by the standard library; check your kitc installation"
        )
        Nothing
      _ -> return ()
    let requiredTypes = [typeOptionPath]
    forMWithErrors_ requiredTypes $ \t -> do
      result <-
        (try $ getTypeDefinition ctx t) :: IO
          (Either KitError (TypeDefinition TypedExpr ConcreteType))
      case result of
        Left _ -> throwk $ InternalError
          ("Sanity check failed: couldn't find required type "
          ++ s_unpack (showTypePath t)
          ++ ", which should be provided by the standard library; check your kitc installation"
          )
          Nothing
        _ -> return ()


decideModuleAndSourcePaths :: Str -> [FilePath] -> (Str, [FilePath])
decideModuleAndSourcePaths mod paths =
  let
    normPaths      = [ dropTrailingPathSeparator $ normalise p | p <- paths ]
    modAsPath      = normalise $ s_unpack mod
    modAsPathNoExt = case stripExtension ".kit" modAsPath of
      Just p  -> p
      Nothing -> modAsPath
    modDir = case init $ splitDirectories modAsPath of
      [] -> "."
      p  -> joinPath p
    normPathsWithExtra =
      (if s_isSuffixOf ".kit" mod && not modReachableAlready
          then [modDir]
          else []
        )
        ++ normPaths
    inputPathsThatCanLeadToMod =
      [ p
      | p <- normPaths
      , isPrefixOf (splitDirectories p) (splitDirectories modAsPath)
      ]
    modPathSplit = s_split '/' mod
    actualMod    = case inputPathsThatCanLeadToMod of
      [] -> s_pack $ takeFileName modAsPathNoExt
      -- What should be done if multiple source paths can lead to the file we specified?
      _ ->
        s_join "."
          $ map s_pack
          $ splitDirectories
          $ makeRelative (head inputPathsThatCanLeadToMod)
          $ modAsPathNoExt
    modReachableAlready = not $ null inputPathsThatCanLeadToMod
  in
    (actualMod, if null normPathsWithExtra then ["src"] else normPathsWithExtra)
