module Kit.Compiler.Passes.GenerateCode where

import Control.Exception
import Control.Monad
import Data.IORef
import Data.List
import Language.C
import System.Directory
import System.FilePath
import System.IO
import Text.PrettyPrint
import Kit.Ast
import Kit.CodeGen.C
import Kit.Compiler.Context
import Kit.Compiler.Module
import Kit.Compiler.Utils
import Kit.Error
import Kit.HashTable
import Kit.Ir
import Kit.Parser
import Kit.Str

generateCode :: CompileContext -> IO ()
generateCode ctx = do
  mods <- h_toList $ ctxModules ctx
  forM_ (map snd mods) (generateModule ctx)
  return ()

generateModule :: CompileContext -> Module -> IO ()
generateModule ctx mod = do
  generateHeader ctx mod
  generateLib    ctx mod

generateHeader ctx mod = do
  let headerFilePath = (includePath ctx $ modPath mod)
  debugLog ctx
    $  "generating header for "
    ++ show mod
    ++ " in "
    ++ headerFilePath
  createDirectoryIfMissing True $ takeDirectory $ headerFilePath
  handle <- openFile headerFilePath WriteMode
  hPutStrLn handle $ "#ifndef " ++ (modDef $ modPath mod)
  hPutStrLn handle $ "#define " ++ (modDef $ modPath mod)
  forM_
    (modIncludes mod)
    (\(filepath, _) -> hPutStrLn handle $ "#include \"" ++ filepath ++ "\"")
  forM_
    (map fst (modImports mod))
    (\imp -> do
      hPutStrLn handle $ "#include \"" ++ (relativeLibPath imp -<.> "h") ++ "\""
    )
  ir <- readIORef (modIr mod)
  forM_     ir     (generateHeaderDecl ctx mod handle)
  hPutStrLn handle "#endif"
  hClose handle

generateLib ctx mod = do
  let codeFilePath = (libPath ctx $ modPath mod)
  debugLog ctx $ "generating code for " ++ show mod ++ " in " ++ codeFilePath
  -- create output directories
  createDirectoryIfMissing True $ takeDirectory $ codeFilePath
  handle <- openFile codeFilePath WriteMode
  hPutStrLn handle
    $  "#include \""
    ++ (relativeLibPath (modPath mod) -<.> "h")
    ++ "\""
  ir <- readIORef (modIr mod)
  forM_ ir (generateDecl ctx mod handle)
  hClose handle

relativeLibPath :: ModulePath -> FilePath
relativeLibPath mod = moduleFilePath mod -<.> ""

modDef :: ModulePath -> String
modDef m = "KIT_INCLUDE__" ++ (intercalate "__" (map s_unpack m))

includePath :: CompileContext -> ModulePath -> FilePath
includePath ctx mod =
  ((ctxOutputDir ctx) </> "include" </> (moduleFilePath mod -<.> ".h"))

libPath :: CompileContext -> ModulePath -> FilePath
libPath ctx mod =
  ((ctxOutputDir ctx) </> "lib" </> (moduleFilePath mod -<.> ".c"))

generateHeaderDecl :: CompileContext -> Module -> Handle -> IrDecl -> IO ()
generateHeaderDecl ctx mod headerFile decl = do
  case decl of
    IrFunction (FunctionDefinition { functionName = name, functionType = t, functionBody = Just body, functionNameMangling = mangle })
      -> do
        let name' = mangleName mangle name
        hPutStrLn headerFile (render $ pretty $ CDeclExt $ cfunDecl name' t)

generateDecl :: CompileContext -> Module -> Handle -> IrDecl -> IO ()
generateDecl ctx mod codeFile decl = do
  case decl of
    IrFunction (FunctionDefinition { functionName = name, functionType = t, functionBody = Just body, functionNameMangling = mangle })
      -> do
        let name' = mangleName mangle name
        hPutStrLn codeFile (render $ pretty $ cfunDef name' t body)
