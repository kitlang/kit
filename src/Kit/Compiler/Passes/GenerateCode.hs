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
    let codeFilePath = (libPath ctx $ mod_path mod)
    let headerFilePath = (includePath ctx $ mod_path mod)
    -- create output directories
    createDirectoryIfMissing True $ takeDirectory $ codeFilePath
    createDirectoryIfMissing True $ takeDirectory $ headerFilePath
    codeFile <- openFile codeFilePath WriteMode
    headerFile <- openFile headerFilePath WriteMode
    -- open file and write imports/includes
    writeHeader ctx mod headerFile True
    writeHeader ctx mod codeFile False
    -- transpile and write code/declarations
    ir <- readIORef (mod_ir mod)
    forM_ ir (generateDecl ctx mod codeFile headerFile)
    -- finish and close files
    writeFooter ctx mod headerFile True
    writeFooter ctx mod codeFile False
    return ()

  writeHeader :: CompileContext -> Module -> Handle -> Bool -> IO ()
  writeHeader ctx mod handle cheader = do
    if cheader
      then hPutStrLn handle $ "#ifndef " ++ (modDef $ mod_path mod) ++ "\n#define " ++ (modDef $ mod_path mod) ++ "\n";
      else return ()
    forM_ (mod_includes mod) (\(filepath, _) -> hPutStrLn handle $ "#include \"" ++ filepath ++ "\"")
    forM_ ((mod_path mod) : (map fst (mod_imports mod))) (\imp -> hPutStrLn handle $ "#ifndef " ++ (modDef imp) ++ "\n#include \"" ++ (relativeLibPath imp -<.> "h") ++ "\"\n#endif")

  writeFooter :: CompileContext -> Module -> Handle -> Bool -> IO ()
  writeFooter ctx mod handle cheader = do
    if cheader
      then hPutStrLn handle "#endif"
      else return ()
    hClose handle

  relativeLibPath :: ModulePath -> FilePath
  relativeLibPath mod = moduleFilePath mod -<.> ""

  modDef :: ModulePath -> String
  modDef m = "KIT_INCLUDE__" ++ (intercalate "__" (map s_unpack m))

  includePath :: CompileContext -> ModulePath -> FilePath
  includePath ctx mod = ((ctxOutputDir ctx) </> "include" </> (moduleFilePath mod -<.> ".h"))

  libPath :: CompileContext -> ModulePath -> FilePath
  libPath ctx mod = ((ctxOutputDir ctx) </> "lib" </> (moduleFilePath mod -<.> ".c"))

  generateDecl :: CompileContext -> Module -> Handle -> Handle -> IrDecl -> IO ()
  generateDecl ctx mod codeFile headerFile decl = do
    case decl of
      IrFunction name t body -> do
        hPutStrLn headerFile (render $ pretty $ CDeclExt $ cfunDecl name t)
        hPutStrLn codeFile (render $ pretty $ cfunDef name t body)
