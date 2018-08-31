module Kit.Compiler.Passes.GenerateCode where

import Control.Exception
import Control.Monad
import Data.IORef
import Data.List
import Data.Maybe
import Language.C
import System.Directory
import System.FilePath
import System.IO
import Text.PrettyPrint
import Kit.Ast
import Kit.CodeGen.C
import Kit.Compiler.Context
import Kit.Compiler.Generators.DeclIr
import Kit.Compiler.Generators.NameMangling
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
generateCode :: CompileContext -> [(Module, [DeclBundle])] -> IO [TypePath]
generateCode ctx ir = do
  forM_ [libDir ctx, includeDir ctx, objDir ctx] $ \d -> do
    exists <- doesDirectoryExist d
    when exists $ removeDirectoryRecursive d
  -- memoize bundle names; assumption is that subpaths, if they don't have
  -- their own bundle, will be provided by a parent
  bundlesMemo <- h_new
  forM_ ir $ \(_, bundles) ->
    forM_ bundles $ \x -> h_insert bundlesMemo (bundleTp x) ()
  mods  <- ctxSourceModules ctx
  names <- forM ir $ generateModule ctx bundlesMemo
  return $ foldr (++) [] names

generateModule
  :: CompileContext
  -> HashTable TypePath ()
  -> (Module, [DeclBundle])
  -> IO [TypePath]
generateModule ctx memo (mod, bundles) =
  forM bundles $ generateBundle ctx mod memo

generateBundle
  :: CompileContext
  -> Module
  -> HashTable TypePath ()
  -> DeclBundle
  -> IO TypePath
generateBundle ctx mod memo (DeclBundle name decls deps) = do
  generateBundleHeader ctx mod name decls memo deps
  generateBundleLib ctx name decls
  return name

generateBundleHeader
  :: CompileContext
  -> Module
  -> TypePath
  -> [IrDecl]
  -> HashTable TypePath ()
  -> [IncludeDependency]
  -> IO ()
generateBundleHeader ctx mod name decls bundles deps = do
  let headerFilePath = includePath ctx name
  debugLog ctx
    $  "generating header for "
    ++ s_unpack (showTypePath name)
    ++ " in "
    ++ headerFilePath
  createDirectoryIfMissing True $ takeDirectory $ headerFilePath
  handle <- openFile headerFilePath WriteMode
  hPutStrLn handle $ "#ifndef " ++ (bundleDef $ mangleName name)
  hPutStrLn handle $ "#define " ++ (bundleDef $ mangleName name)
  -- native includes
  includes <- readIORef (modIncludes mod)
  forM_
    includes
    (\(filepath, _) -> hPutStrLn handle $ "#include \"" ++ filepath ++ "\"")
  -- forward declare structure names
  forM_ decls $ generateHeaderForwardDecl ctx handle
  -- imports
  deps <- forM deps $ \dep -> case dep of
    DeclDependency t ->
      let decl = cDecl t Nothing Nothing
      in  return $ Just $ render $ pretty $ CDeclExt $ decl
    DefDependency tp -> do
      found <- findBundleFor bundles tp
      case found of
        -- stop including yourself! stop including yourself!
        Just x | x == name -> return Nothing
        Just x ->
          return
            $  Just
            $  "#include \""
            ++ (makeRelative (includeDir ctx) $ includePath ctx $ x)
            ++ "\""
        _ -> return Nothing
  forM_ (nub $ catMaybes deps) $ hPutStrLn handle
  -- definitions
  forM_ decls $ generateHeaderDef ctx handle
  hPutStrLn handle "#endif"
  hClose handle

findBundleFor :: HashTable TypePath () -> TypePath -> IO (Maybe TypePath)
findBundleFor bundles b = do
  existing <- h_lookup bundles b
  case existing of
    Just x  -> return $ Just b
    Nothing -> case b of
      ([], s) -> return Nothing
      (n , s) -> findBundleFor bundles (tpShift b)

bundleDef :: Str -> String
bundleDef s = "KIT_INCLUDE__" ++ s_unpack s

generateHeaderForwardDecl :: CompileContext -> Handle -> IrDecl -> IO ()
generateHeaderForwardDecl ctx headerFile decl = do
  case decl of
    DeclTuple t@(BasicTypeTuple name _) -> do
      hPutStrLn
        headerFile
        (render $ pretty $ CDeclExt $ cDecl (BasicTypeTuple name [])
                                            Nothing
                                            Nothing
        )

    DeclType def@(TypeDefinition { typeSubtype = Atom }) -> return ()

    DeclType def@(TypeDefinition { typeName = name }   ) -> do
      case typeBasicType def of
        Just x ->
          let decl = cDecl x Nothing Nothing
          in  hPutStrLn headerFile (render $ pretty $ CDeclExt $ decl)
        _ -> return ()

    _ -> do
      return ()

generateHeaderDef :: CompileContext -> Handle -> IrDecl -> IO ()
generateHeaderDef ctx headerFile decl = case decl of
  DeclTuple (BasicTypeTuple name slots) -> do
    let decls = cTupleDecl name slots
    mapM_ (\d -> hPutStrLn headerFile (render $ pretty $ CDeclExt d)) decls

  DeclType def@(TypeDefinition { typeSubtype = Atom }) -> return ()

  DeclType def@(TypeDefinition{}                     ) -> do
    let decls = cTypeDecl def
    mapM_ (\d -> hPutStrLn headerFile (render $ pretty $ CDeclExt d)) decls

  DeclFunction def@(FunctionDefinition { functionName = name, functionType = t, functionArgs = args, functionVarargs = varargs })
    -> do
      hPutStrLn
        headerFile
        (render $ pretty $ CDeclExt $ cfunDecl name (functionBasicType def))

  DeclVar def@(VarDefinition { varName = name, varType = t }) -> hPutStrLn
    headerFile
    (render $ pretty $ CDeclExt $ cDecl t (Just name) Nothing)

  _ -> return ()

generateBundleLib :: CompileContext -> TypePath -> [IrDecl] -> IO ()
generateBundleLib ctx name decls = do
  let codeFilePath = libPath ctx name
  debugLog ctx
    $  "generating code for "
    ++ (s_unpack $ showTypePath name)
    ++ " in "
    ++ codeFilePath
  -- create output directories
  createDirectoryIfMissing True $ takeDirectory $ codeFilePath
  handle <- openFile codeFilePath WriteMode
  hPutStrLn handle
    $  "#include \""
    ++ (makeRelative (includeDir ctx) $ includePath ctx name)
    ++ "\""
  forM_ decls (generateDef ctx handle)
  hClose handle

generateDef :: CompileContext -> Handle -> IrDecl -> IO ()
generateDef ctx codeFile decl = do
  case decl of
    DeclFunction def@(FunctionDefinition { functionName = name, functionType = t, functionBody = Just body })
      -> do
        hPutStrLn
          codeFile
          ("\n" ++ (render $ pretty $ cfunDef name (functionBasicType def) body)
          )

    DeclVar def@(VarDefinition { varName = name, varType = t@(BasicTypeStruct n), varDefault = Just val })
      -> do
        -- somewhat arbitrarily, compound literals aren't supported as static initializers in GCC <= 4
        hPutStrLn
          codeFile
          (  "\n"
          ++ (render $ pretty $ CDeclExt $ cDecl
               t
               (Just name)
               (Just $ case (transpileExpr val) of
                 CCompoundLit _ x _ -> u $ CInitList $ x
                 x                  -> u $ CInitExpr $ x
               )
             )
          )

    DeclVar def@(VarDefinition { varName = name, varType = t, varDefault = Just val })
      -> do
        hPutStrLn
          codeFile
          (  "\n"
          ++ (render $ pretty $ CDeclExt $ cDecl
               t
               (Just name)
               (Just $ u $ CInitExpr $ transpileExpr val)
             )
          )

    _ -> do
      return ()

functionBasicType :: FunctionDefinition IrExpr BasicType -> BasicType
functionBasicType (FunctionDefinition { functionType = t, functionArgs = args, functionVarargs = varargs })
  = (BasicTypeFunction t (map (\arg -> (argName arg, argType arg)) args) varargs
    )

typeBasicType :: TypeDefinition IrExpr BasicType -> Maybe BasicType
typeBasicType def@(TypeDefinition { typeName = name }) =
  case typeSubtype def of
    Struct { structFields = fields } -> Just $ BasicTypeStruct name
    Union { unionFields = fields }   -> Just $ BasicTypeUnion name
    Enum { enumVariants = variants } -> if all variantIsSimple variants
      then Just $ BasicTypeSimpleEnum name
      else Just $ BasicTypeComplexEnum name
    _ -> Nothing
