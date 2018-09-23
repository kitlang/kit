module Kit.Compiler.Passes.GenerateCode where

import Control.Exception
import Control.Monad
import Data.IORef
import Data.List
import Data.Maybe
import Data.Ord
import Language.C
import System.Directory
import System.FilePath
import System.IO
import Text.PrettyPrint
import Kit.Ast
import Kit.CodeGen.C
import Kit.Compiler.Context
import Kit.Compiler.Generators.DeclIr
import Kit.NameMangling
import Kit.Compiler.Module
import Kit.Compiler.Utils
import Kit.Error
import Kit.HashTable
import Kit.Ir
import Kit.Parser
import Kit.Str

{-
  Generates C code.

  Declarations are bundled into compilation units; to make dependency
  analysis and cross-dependencies easier, a single header is generated for the
  entire project and included from all c files. The header will contain type
  definitions and variable/function declarations.
-}
generateCode :: CompileContext -> [(Module, [DeclBundle])] -> IO [TypePath]
generateCode ctx ir = do
  forM_ [libDir ctx, includeDir ctx, objDir ctx] $ \d -> do
    exists <- doesDirectoryExist d
    when exists $ removeDirectoryRecursive d
  generateProjectHeader ctx ir
  names <- forM ir $ generateModule ctx
  return $ catMaybes $ foldr (++) [] names

generateProjectHeader :: CompileContext -> [(Module, [DeclBundle])] -> IO ()
generateProjectHeader ctx ir = do
  let headerFilePath = includePath ctx
  debugLog ctx $ "generating project header in " ++ headerFilePath
  createDirectoryIfMissing True $ takeDirectory $ headerFilePath
  handle   <- openFile headerFilePath WriteMode
  -- include native dependencies
  includes <- readIORef $ ctxIncludes ctx
  forM_ (nub includes) $ \filepath -> do
    hPutStrLn handle $ "#include \"" ++ filepath ++ "\""
  let flatDecls = foldr
        (++)
        []
        [ bundleMembers bundle | (_, bundles) <- ir, bundle <- bundles ]
  let forwardDecls = map generateHeaderForwardDecl flatDecls
  forM_ (nub $ catMaybes forwardDecls) $ hPutStrLn handle
  sorted <- sortHeaderDefs flatDecls
  let defs = map generateHeaderDef sorted
  forM_ (catMaybes defs) $ hPutStrLn handle
  hClose handle
  return ()

sortHeaderDefs :: [IrDecl] -> IO [IrDecl]
sortHeaderDefs decls = do
  memos        <- h_newSized (length decls)
  dependencies <- h_newSized (length decls)
  -- memoize BasicType dependencies of type declarations
  forM_ decls $ \decl -> case decl of
    DeclType t -> case typeSubtype t of
      Struct { structFields = fields } ->
        h_insert dependencies (typeName t) (map varType fields)
      Union { unionFields = fields } ->
        h_insert dependencies (typeName t) (map varType fields)
      Enum { enumVariants = variants } -> h_insert
        dependencies
        (typeName t)
        (nub [ argType arg | variant <- variants, arg <- variantArgs variant ])
      _ -> return ()
    _ -> return ()
  scored <- forM decls $ \decl -> do
    score <- btOrder dependencies memos (declToBt decl)
    return (if score == (-1) then (1 / 0) else realToFrac score, decl)
  return $ map snd $ sortBy
    (\(a, _) (b, _) ->
      if a > b then GT else LT
    )
    (nub scored)

declToBt :: IrDecl -> BasicType
declToBt (DeclTuple t) = t
declToBt (DeclType  t) = case typeBasicType t of
  Just x  -> x
  Nothing -> BasicTypeUnknown
declToBt t = BasicTypeUnknown

btOrder
  :: HashTable TypePath [BasicType]
  -> HashTable BasicType Int
  -> BasicType
  -> IO Int
btOrder dependencies memos t = do
  let depScore deps = (foldr max (-1) deps) + 1
  let tpScore tp = do
        deps <- h_lookup dependencies tp
        case deps of
          Just x -> do
            deps <- forM x $ btOrder dependencies memos
            return $ (depScore deps) + 1
          Nothing -> return 0
  existing <- h_lookup memos t
  case existing of
    Just x -> return x
    _      -> do
      score <- case t of
        BasicTypeTuple _ fields -> do
          deps <- forM fields $ btOrder dependencies memos
          return $ depScore deps
        BasicTypeStruct      tp -> tpScore tp
        BasicTypeUnion       tp -> tpScore tp
        BasicTypeSimpleEnum tp -> tpScore tp
        BasicTypeComplexEnum tp -> tpScore tp
        _                       -> return (-1)
      h_insert memos t score
      return score

generateModule
  :: CompileContext -> (Module, [DeclBundle]) -> IO [Maybe TypePath]
generateModule ctx (mod, bundles) = forM bundles $ generateBundle ctx mod

bundleNeedsLib :: DeclBundle -> Bool
bundleNeedsLib bundle = foldr
  (\v acc ->
    acc
      || (case v of
           DeclVar      _ -> True
           DeclFunction _ -> True
           _              -> False
         )
  )
  False
  (bundleMembers bundle)

generateBundle :: CompileContext -> Module -> DeclBundle -> IO (Maybe TypePath)
generateBundle ctx mod bundle@(DeclBundle name decls) = do
  -- if a bundle only contains type definitions, no further implementation
  -- is needed
  if bundleNeedsLib bundle
    then do
      generateBundleLib ctx name decls
      return $ Just name
    else return Nothing

bundleDef :: Str -> String
bundleDef s = "KIT_INCLUDE__" ++ s_unpack s

generateHeaderForwardDecl :: IrDecl -> Maybe String
generateHeaderForwardDecl decl = case decl of
  DeclTuple t -> generateTypeForwardDecl t
  DeclType  def@(TypeDefinition { typeSubtype = Atom }) -> Nothing
  DeclType  def@(TypeDefinition { typeName = name }   ) -> do
    case typeBasicType def of
      -- ISO C forbids forward references to enum types
      Just t  -> generateTypeForwardDecl t
      Nothing -> Nothing
  _ -> Nothing

generateTypeForwardDecl :: BasicType -> Maybe String
generateTypeForwardDecl t = case t of
  BasicTypeAnonEnum    _ -> Nothing
  BasicTypeSimpleEnum  _ -> Nothing
  BasicTypeComplexEnum _ -> Nothing
  _ -> Just (render $ pretty $ CDeclExt $ cDecl t Nothing Nothing)

generateHeaderDef :: IrDecl -> Maybe String
generateHeaderDef decl = case decl of
  DeclTuple (BasicTypeTuple name slots) ->
    let decls = cTupleDecl name slots
    in  Just $ intercalate "\n" $ map (\d -> render $ pretty $ CDeclExt d) decls

  DeclType def@(TypeDefinition { typeSubtype = Atom }) -> Nothing

  DeclType def@(TypeDefinition{}) ->
    let decls = cTypeDecl def
    in  Just $ intercalate "\n" $ map (\d -> render $ pretty $ CDeclExt d) decls

  DeclFunction def@(FunctionDefinition { functionType = t, functionArgs = args, functionVarargs = varargs })
    -> Just $ render $ pretty $ CDeclExt $ cfunDecl (functionName def)
                                                    (functionBasicType def)

  DeclVar def@(VarDefinition { varType = t }) ->
    Just $ render $ pretty $ CDeclExt $ cDecl t (Just $ varRealName def) Nothing

  _ -> Nothing

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
    ++ (makeRelative (includeDir ctx) $ includePath ctx)
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
