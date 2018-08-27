module Kit.Compiler.Passes.CompileCode where

import Control.Exception
import Control.Monad
import Data.IORef
import Data.List
import System.Directory
import System.Environment
import System.FilePath
import System.Process
import Kit.Compiler.Context
import Kit.Compiler.Module
import Kit.Compiler.Utils
import Kit.Error
import Kit.HashTable
import Kit.Log
import Kit.Parser
import Kit.Str

{-
  Compile the generated C code.
-}
compileCode :: CompileContext -> IO (Maybe FilePath)
compileCode ctx = do
  compiler      <- findCompiler ctx
  ccache        <- findCcache ctx
  compilerFlags <- getCompilerFlags ctx
  linkerFlags   <- getLinkerFlags ctx
  createDirectoryIfMissing True (buildDir ctx)
  debugLog                 ctx  ("found C compiler at " ++ compiler)
  mods <- ctxSourceModules ctx
  forM_ mods (compileModule ctx ccache compiler compilerFlags)
  if ctxNoLink ctx
    then return Nothing
    else do
      binPath <- link ctx compiler linkerFlags mods
      return $ Just binPath

compileModule
  :: CompileContext -> Maybe FilePath -> FilePath -> [String] -> Module -> IO ()
compileModule ctx ccache cc' args mod = do
  let objFilePath = objPath ctx (modPath mod)
  createDirectoryIfMissing True (takeDirectory objFilePath)
  let args' =
        args
          ++ [ "-I" ++ includeDir ctx
             , "-c"
             , libPath ctx (modPath mod)
             , "-o"
             , objPath ctx (modPath mod)
             , "-std=c99"
             ]
          ++ (if (ctxIsLibrary ctx) then ["-fPIC"] else [])
  printLog $ "compiling " ++ show mod
  let (cc, args) = case ccache of
        Just x  -> (x, cc' : args')
        Nothing -> (cc', args')
  traceLog $ showCommandForUser cc args
  callProcess cc args

link :: CompileContext -> FilePath -> [String] -> [Module] -> IO FilePath
link ctx cc args mods = do
  let args' = if (ctxIsLibrary ctx)
        then
          [ objPath ctx (modPath mod) | mod <- mods ]
          ++ ["-I" ++ includeDir ctx, "-shared", "-obuild/main.so"]
          ++ args
        else
          [ objPath ctx (modPath mod) | mod <- mods ]
          ++ ["-I" ++ includeDir ctx, "-obuild/main"]
          ++ args
  printLog $ "linking"
  traceLog $ showCommandForUser cc args'
  callProcess cc args'
  return $ "build" </> "main"

buildDir :: CompileContext -> FilePath
buildDir ctx = (ctxOutputDir ctx)

getCompilerFlags :: CompileContext -> IO [String]
getCompilerFlags ctx = do
  let ctxFlags = ctxCompilerFlags ctx
  envFlags <- lookupEnv "COMPILER_FLAGS"
  return $ case envFlags of
    -- FIXME
    Just s  -> ctxFlags ++ words s
    Nothing -> ctxFlags

getLinkerFlags :: CompileContext -> IO [String]
getLinkerFlags ctx = do
  let ctxFlags = ctxLinkerFlags ctx
  envFlags <- lookupEnv "LINKER_FLAGS"
  return $ case envFlags of
    -- FIXME
    Just s  -> ctxFlags ++ words s
    Nothing -> ctxFlags

findCompiler :: CompileContext -> IO FilePath
findCompiler ctx = do
  case ctxCompilerPath ctx of
    Just cc -> return cc
    _       -> do
      ccEnv <- lookupEnv "CC"
      case ccEnv of
        Just cc -> return cc
        Nothing -> do
          let exes = ["cc", "gcc", "clang"]
          exePaths <- mapM findExecutable exes
          case msum exePaths of
            Just cc -> return cc
            Nothing -> throwk $ BasicError
              ("Couldn't find a C compiler from your executable paths; tried looking for:\n\n"
              ++ intercalate "\n" ([ "  - " ++ exe | exe <- exes ])
              ++ "\n\nYou can set the compiler path explicitly using the CC environment variable"
              )
              Nothing

findCcache :: CompileContext -> IO (Maybe FilePath)
findCcache ctx =
  if ctxNoCcache ctx then return Nothing else findExecutable "ccache"
