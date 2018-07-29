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

{-
  Generates C code and header files from the populated modIr fields of all
  modules.
-}
generateCode :: CompileContext -> IO ()
generateCode ctx = do
  mods <- ctxSourceModules ctx
  forM_ mods (generateModule ctx)
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
  includes <- readIORef (modIncludes mod)
  forM_
    includes
    (\(filepath, _) -> hPutStrLn handle $ "#include \"" ++ filepath ++ "\"")
  forM_
    (map fst (modImports mod))
    (\imp -> do
      hPutStrLn handle $ "#include \"" ++ (relativeLibPath imp -<.> "h") ++ "\""
    )
  ir <- readIORef (modIr mod)
  hPutStrLn handle "\n/* forward declarations */\n"
  forM_     ir     (generateHeaderForwardDecl ctx mod handle)
  hPutStrLn handle "\n/* type and function declarations */\n"
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
  forM_ ir (generateDef ctx mod handle)
  hClose handle

relativeLibPath :: ModulePath -> FilePath
relativeLibPath mod = moduleFilePath mod -<.> ""

modDef :: ModulePath -> String
modDef m = "KIT_INCLUDE__" ++ (intercalate "__" (map s_unpack m))

generateHeaderForwardDecl
  :: CompileContext -> Module -> Handle -> IrDecl -> IO ()
generateHeaderForwardDecl ctx mod headerFile decl = do
  case decl of
    DeclType def@(TypeDefinition { typeSubtype = Atom }) -> return ()

    DeclType def@(TypeDefinition { typeName = name }   ) -> do
      let decl = cDecl (typeBasicType def) Nothing Nothing
      hPutStrLn headerFile (render $ pretty $ CDeclExt $ decl)

    _ -> do
      return ()

generateHeaderDecl :: CompileContext -> Module -> Handle -> IrDecl -> IO ()
generateHeaderDecl ctx mod headerFile decl = do
  case decl of
    DeclType def@(TypeDefinition { typeSubtype = Atom }) -> return ()

    DeclType def@(TypeDefinition{}                     ) -> do
      let decls = cdecl (typeBasicType def)
      mapM_ (\d -> hPutStrLn headerFile (render $ pretty $ CDeclExt $ d)) decls

    DeclFunction def@(FunctionDefinition { functionName = name, functionType = t, functionArgs = args, functionVarargs = varargs })
      -> do
        hPutStrLn
          headerFile
          (render $ pretty $ CDeclExt $ cfunDecl name (functionBasicType def))

    DeclVar def@(VarDefinition { varName = name, varType = t }) -> do
      hPutStrLn headerFile
                (render $ pretty $ CDeclExt $ cDecl t (Just name) Nothing)

    -- TODO: remove
    _ -> do
      return ()

generateDef :: CompileContext -> Module -> Handle -> IrDecl -> IO ()
generateDef ctx mod codeFile decl = do
  case decl of
    DeclFunction def@(FunctionDefinition { functionName = name, functionType = t, functionBody = Just body })
      -> do
        hPutStrLn
          codeFile
          ("\n" ++ (render $ pretty $ cfunDef name (functionBasicType def) body)
          )

    DeclVar def@(VarDefinition { varName = name, varType = t, varDefault = Just val })
      -> do
        hPutStrLn
          codeFile
          ("\n" ++ (render $ pretty $ CDeclExt $ cDecl t (Just name) (Just $ u $ CInitExpr $ transpileExpr val)))

    _ -> do
      return ()

functionBasicType :: FunctionDefinition IrExpr BasicType -> BasicType
functionBasicType (FunctionDefinition { functionType = t, functionArgs = args, functionVarargs = varargs })
  = (BasicTypeFunction t (map (\arg -> (argName arg, argType arg)) args) varargs
    )

typeBasicType :: TypeDefinition IrExpr BasicType -> BasicType
typeBasicType def@(TypeDefinition { typeName = name }) =
  case typeSubtype def of
    Struct { structFields = fields } -> BasicTypeStruct
      (Just name)
      [ (varName field, varType field) | field <- fields ]
    Union { unionFields = fields } -> BasicTypeUnion
      (Just name)
      [ (varName field, varType field) | field <- fields ]
    Enum { enumVariants = variants } -> if all variantIsSimple variants
      then BasicTypeSimpleEnum (Just name) [ variantName v | v <- variants ]
      else BasicTypeComplexEnum
        name
        [ (variantName v, [ (argName a, argType a) | a <- variantArgs v ])
        | v <- variants
        ]
