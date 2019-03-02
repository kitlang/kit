module Kit.Compiler.Passes.CompileCode (compileCode) where

import Control.Concurrent.Async
import Control.Monad
import Data.Either
import Data.List
import Data.Mutable
import System.Directory
import System.FilePath
import System.Process
import Kit.Ast
import Kit.Compiler.Context
import Kit.Compiler.Utils
import Kit.Error
import Kit.Log
import Kit.Str
import Kit.Toolchain

{-
  Compile the generated C code.
-}
compileCode :: CompileContext -> Toolchain -> [TypePath] -> IO (Maybe FilePath)
compileCode ctx cc names = do
  createDirectoryIfMissing True (buildDir ctx)
  results <- forConcurrently names (compileBundle ctx cc)
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
      binPath <- linkBundles ctx cc names
      return $ Just binPath

compileBundle
  :: CompileContext
  -> Toolchain
  -> TypePath
  -> IO (Either String String)
compileBundle ctx cc name = do
  let objFilePath = objPath ctx name
  createDirectoryIfMissing True (takeDirectory objFilePath)
  debugLog ctx $ "compiling " ++ s_unpack (showTypePath name)
  invokeCompiler (cc {ccIncludePaths = (includeDir ctx) : (ccIncludePaths cc)}) (debugLog ctx) (not $ ctxNoCcache ctx) (libPath ctx name) (objPath ctx name)

linkBundles
  :: CompileContext -> Toolchain -> [TypePath] -> IO FilePath
linkBundles ctx cc names = do
  let outName = ctxOutputPath ctx
  when (ctxVerbose ctx >= 0) $ printLog $ "linking"
  linkedLibs <- readRef $ ctxLinkedLibs ctx
  invokeLinker (cc {
      ccIncludePaths = (includeDir ctx) : (ccIncludePaths cc),
      ldFlags = (ldFlags cc) ++ [ "-l" ++ s_unpack lib | lib <- nub linkedLibs ]
    }) (debugLog ctx) (ctxIsLibrary ctx) [ objPath ctx name | name <- names ] outName
