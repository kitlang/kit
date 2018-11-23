module Kit.Compiler.Passes.CompileCode where

import Control.Monad
import Data.List
import System.Directory
import System.FilePath
import System.Process
import Kit.Ast
import Kit.Compiler.CCompiler
import Kit.Compiler.Context
import Kit.Compiler.Utils
import Kit.Log
import Kit.Str

{-
  Compile the generated C code.
-}
compileCode :: CompileContext -> CCompiler -> [TypePath] -> IO (Maybe FilePath)
compileCode ctx cc names = do
  ccache        <- findCcache ctx
  compilerFlags <- getCompileFlags ctx cc
  linkerFlags   <- getCtxLinkerFlags ctx
  createDirectoryIfMissing True  (buildDir ctx)
  forM_                    names (compileBundle ctx ccache cc compilerFlags)
  if ctxNoLink ctx
    then return Nothing
    else do
      binPath <- link ctx cc linkerFlags names
      return $ Just binPath

compileBundle
  :: CompileContext
  -> Maybe FilePath
  -> CCompiler
  -> [String]
  -> TypePath
  -> IO ()
compileBundle ctx ccache cc args name = do
  let objFilePath = objPath ctx name
  createDirectoryIfMissing True (takeDirectory objFilePath)
  args <-
    return
    $  args
    ++ ["-I" ++ includeDir ctx, "-c", libPath ctx name, "-o", objPath ctx name]
  when (ctxVerbose ctx >= 0) $ printLog $ "compiling " ++ s_unpack
    (showTypePath name)
  (ccPath, args) <- return $ case ccache of
    Just x  -> (x, ccPath cc : args)
    Nothing -> (ccPath cc, args)
  when (ctxVerbose ctx >= 0) $ traceLog $ showCommandForUser ccPath args
  callProcess ccPath args

link :: CompileContext -> CCompiler -> [String] -> [TypePath] -> IO FilePath
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
  when (ctxVerbose ctx >= 0) $ traceLog $ showCommandForUser (ccPath cc) args'
  callProcess (ccPath cc) args'
  binPath <- canonicalizePath outName
  return $ binPath

compilerSpecificArgs :: FilePath -> [String]
compilerSpecificArgs "clang" = ["-Wno-error=unused-command-line-argument"]
compilerSpecificArgs _       = []
