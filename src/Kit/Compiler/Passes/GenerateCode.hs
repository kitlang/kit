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
generateCode :: CompileContext -> [(Module, [IrDecl])] -> IO ()
generateCode ctx ir = do
  mods <- ctxSourceModules ctx
  forM_ ir (generateModule ctx)
  return ()

generateModule :: CompileContext -> (Module, [IrDecl]) -> IO ()
generateModule ctx (mod, decls) = do
  generateHeader ctx mod decls
  generateLib    ctx mod decls

generateHeader ctx mod decls = do
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

  hPutStrLn handle "\n/* forward declarations */\n"
  forM_     decls  (generateHeaderForwardDecl ctx mod handle)
  hPutStrLn handle "\n/* type and function declarations */\n"
  sorted <- sortHeaderDefs decls
  forM_     sorted (generateHeaderDecl ctx mod handle)
  hPutStrLn handle "#endif"
  hClose handle

generateLib ctx mod decls = do
  let codeFilePath = (libPath ctx $ modPath mod)
  debugLog ctx $ "generating code for " ++ show mod ++ " in " ++ codeFilePath
  -- create output directories
  createDirectoryIfMissing True $ takeDirectory $ codeFilePath
  handle <- openFile codeFilePath WriteMode
  hPutStrLn handle
    $  "#include \""
    ++ (relativeLibPath (modPath mod) -<.> "h")
    ++ "\""
  forM_ decls (generateDef ctx mod handle)
  hClose handle

relativeLibPath :: ModulePath -> FilePath
relativeLibPath mod = moduleFilePath mod -<.> ""

modDef :: ModulePath -> String
modDef m = "KIT_INCLUDE__" ++ (intercalate "__" (map s_unpack m))

generateHeaderForwardDecl
  :: CompileContext -> Module -> Handle -> IrDecl -> IO ()
generateHeaderForwardDecl ctx mod headerFile decl = do
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

sortHeaderDefs :: [IrDecl] -> IO [IrDecl]
sortHeaderDefs decls = do
  memos  <- h_newSized (length decls)
  scored <- forM decls $ \decl -> do
    score <- btOrder memos (declToBt decl)
    return (score, decl)
  return $ map snd $ sortBy
    (\(a, _) (b, _) ->
      if a == b then EQ else if (a == -1) || a > b then GT else LT
    )
    scored

declToBt :: IrDecl -> BasicType
declToBt (DeclTuple t) = t
declToBt (DeclType  t) = case typeBasicType t of
  Just x  -> x
  Nothing -> BasicTypeUnknown
declToBt t = BasicTypeUnknown

btOrder :: HashTable BasicType Int -> BasicType -> IO Int
btOrder memos t = do
  let depScore deps = (foldr max (-1) deps) + 1
  existing <- h_lookup memos t
  case existing of
    Just x -> return x
    _      -> do
      score <- case t of
        BasicTypeTuple _ fields -> do
          deps <- forM fields $ btOrder memos
          return $ depScore deps
        -- BasicTypeStruct _ args -> do
        --   deps <- forM (map snd args) $ btOrder memos
        --   return $ depScore deps
        -- BasicTypeUnion _ args -> do
        --   deps <- forM (map snd args) $ btOrder memos
        --   return $ depScore deps
        _ -> return (-1)
      h_insert memos t score
      return score

generateHeaderDecl :: CompileContext -> Module -> Handle -> IrDecl -> IO ()
generateHeaderDecl ctx mod headerFile decl = do
  case decl of
    DeclTuple (BasicTypeTuple name slots) -> do
      hPutStrLn headerFile $ "#ifndef KIT_TUPLE__" ++ s_unpack name
      hPutStrLn headerFile $ "#define KIT_TUPLE__" ++ s_unpack name
      let decls = cTupleDecl name slots
      mapM_ (\d -> hPutStrLn headerFile (render $ pretty $ CDeclExt d)) decls
      hPutStrLn headerFile $ "#endif"

    DeclType def@(TypeDefinition { typeSubtype = Atom }) -> return ()

    DeclType def@(TypeDefinition{}                     ) -> do
      let decls = cTypeDecl def
      mapM_ (\d -> hPutStrLn headerFile (render $ pretty $ CDeclExt d)) decls

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
