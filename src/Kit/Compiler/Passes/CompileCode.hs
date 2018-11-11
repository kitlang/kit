module Kit.Compiler.Passes.CompileCode where

import Control.Monad
import Data.List
import System.Directory
import System.FilePath
import System.Process
import Kit.Ast
import Kit.Compiler.Context
import Kit.Compiler.Utils
import Kit.Log
import Kit.Str

{-
  Compile the generated C code.
-}
compileCode :: CompileContext -> [TypePath] -> IO (Maybe FilePath)
compileCode ctx names = do
  compiler      <- findCompiler ctx
  ccache        <- findCcache ctx
  compilerFlags <- getCompilerFlags ctx
  linkerFlags   <- getLinkerFlags ctx
  createDirectoryIfMissing True (buildDir ctx)
  debugLog ctx ("found C compiler at " ++ compiler)
  forM_ names (compileBundle ctx ccache compiler compilerFlags)
  if ctxNoLink ctx
    then return Nothing
    else do
      binPath <- link ctx compiler linkerFlags names
      return $ Just binPath

compileBundle
  :: CompileContext
  -> Maybe FilePath
  -> FilePath
  -> [String]
  -> TypePath
  -> IO ()
compileBundle ctx ccache cc' args name = do
  let objFilePath = objPath ctx name
  createDirectoryIfMissing True (takeDirectory objFilePath)
  let args' =
        args
          ++ [ "-I" ++ includeDir ctx
             , "-c"
             , libPath ctx name
             , "-o"
             , objPath ctx name
             ]
          ++ (defaultCompileArgs ctx $ takeFileName cc')
          ++ (compilerSpecificArgs $ takeFileName cc')
  when (ctxVerbose ctx >= 0) $ printLog $ "compiling " ++ s_unpack
    (showTypePath name)
  let (cc, args) = case ccache of
        Just x  -> (x, cc' : args')
        Nothing -> (cc', args')
  when (ctxVerbose ctx >= 0) $ traceLog $ showCommandForUser cc args
  callProcess cc args

link :: CompileContext -> FilePath -> [String] -> [TypePath] -> IO FilePath
link ctx cc args names = do
  let outName = ctxOutputPath ctx
  let args' =
        [ objPath ctx name | name <- names ]
          ++ ["-I" ++ includeDir ctx]
          ++ (if ctxIsLibrary ctx
               then ["-shared", "-o" ++ outName]
               else ["-o" ++ outName]
             )
          ++ args
  when (ctxVerbose ctx >= 0) $ printLog $ "linking"
  when (ctxVerbose ctx >= 0) $ traceLog $ showCommandForUser cc args'
  callProcess cc args'
  binPath <- canonicalizePath outName
  return $ binPath

compilerSpecificArgs :: FilePath -> [String]
compilerSpecificArgs "clang" = ["-Wno-error=unused-command-line-argument"]
compilerSpecificArgs _       = []
