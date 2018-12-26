module Kit.Compiler.Passes.CompileCode (compileCode) where

import Control.Concurrent.Async
import Control.Monad
import Data.Either
import Data.List
import System.Directory
import System.Exit
import System.FilePath
import System.Process
import Kit.Ast
import Kit.Compiler.CCompiler
import Kit.Compiler.Context
import Kit.Compiler.Utils
import Kit.Error
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
  createDirectoryIfMissing True (buildDir ctx)
  results <- forConcurrently names (compileBundle ctx ccache cc compilerFlags)
  errs    <- foldM
    (\acc out -> case out of
      Left  err -> return $ (BasicError err Nothing) : acc
      Right s   -> do
        putStr s
        return acc
    )
    []
    results
  when (not $ null errs) $ throwk $ KitErrors $ reverse $ map KitError errs
  if ctxNoLink ctx
    then return Nothing
    else do
      binPath <- linkBundles ctx cc linkerFlags names
      return $ Just binPath

compileBundle
  :: CompileContext
  -> Maybe FilePath
  -> CCompiler
  -> [String]
  -> TypePath
  -> IO (Either String String)
compileBundle ctx ccache cc args name = do
  let objFilePath = objPath ctx name
  createDirectoryIfMissing True (takeDirectory objFilePath)
  args <-
    return
    $  args
    ++ ["-I" ++ includeDir ctx, "-c", libPath ctx name, "-o", objPath ctx name]
  debugLog ctx $ "compiling " ++ s_unpack (showTypePath name)
  (ccPath, args) <- return $ case ccache of
    Just x  -> (x, ccPath cc : args)
    Nothing -> (ccPath cc, args)
  debugLog ctx $ showCommandForUser ccPath args
  (status, _, stderr) <- readCreateProcessWithExitCode (proc ccPath args)
                                                            ""
  if status == ExitSuccess
    then return $ Right stderr
    else return $ Left stderr

linkBundles
  :: CompileContext -> CCompiler -> [String] -> [TypePath] -> IO FilePath
linkBundles ctx cc args names = do
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
  debugLog ctx $ showCommandForUser (ccPath cc) args'
  callProcess (ccPath cc) args'
  binPath <- canonicalizePath outName
  return $ binPath

compilerSpecificArgs :: FilePath -> [String]
compilerSpecificArgs "clang" = ["-Wno-error=unused-command-line-argument"]
compilerSpecificArgs _       = []
