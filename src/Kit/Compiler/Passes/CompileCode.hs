module Kit.Compiler.Passes.CompileCode where

import Control.Exception
import Control.Monad
import Data.IORef
import Data.List
import System.Directory
import System.Environment
import System.FilePath
import System.Process
import Kit.Ast
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
             , "-D_GNU_SOURCE"
             , "-D_BSD_SOURCE"
             , "-D_DEFAULT_SOURCE"
             , "-std=c99"
             , "-pedantic"
             , "-O3"
             , "-Os"
             ]
          ++ (if (ctxIsLibrary ctx) then ["-fPIC"] else [])
          ++ compilerSpecificArgs cc'
  printLog $ "compiling " ++ s_unpack (showTypePath name)
  let (cc, args) = case ccache of
        Just x  -> (x, cc' : args')
        Nothing -> (cc', args')
  traceLog $ showCommandForUser cc args
  callProcess cc args

link :: CompileContext -> FilePath -> [String] -> [TypePath] -> IO FilePath
link ctx cc args names = do
  let args' =
        [ objPath ctx name | name <- names ]
          ++ ["-I" ++ includeDir ctx]
          ++ (if ctxIsLibrary ctx
               then ["-shared", "-obuild/main.so"]
               else ["-obuild/main"]
             )
          ++ args
  printLog $ "linking"
  traceLog $ showCommandForUser cc args'
  callProcess cc args'
  return $ "build" </> "main"

compilerSpecificArgs :: FilePath -> [String]
compilerSpecificArgs "clang" = ["-Wno-error=unused-command-line-argument"]
compilerSpecificArgs _       = []
