module Kit.Compiler.Generators.C.GenerateCModule where

import Control.Monad
import Data.List
import Data.Maybe
import Language.C
import System.Directory
import System.FilePath
import System.IO
import Text.PrettyPrint
import Kit.Ast
import Kit.Compiler.Context
import Kit.Compiler.Generators.C.CExpr
import Kit.Compiler.Generators.C.CFun
import Kit.Compiler.Generators.C.GenerateCHeader
import Kit.Compiler.Module
import Kit.Compiler.Utils
import Kit.Ir
import Kit.Str

generateModule :: CompileContext -> (Module, [IrBundle]) -> IO [Maybe TypePath]
generateModule ctx (mod, bundles) = forM bundles $ generateBundle ctx mod

bundleNeedsLib :: IrBundle -> Bool
bundleNeedsLib bundle = foldr
  (\v acc ->
    acc
      || (case stmt v of
           VarDeclaration      _ -> True
           FunctionDeclaration _ -> True
           _                     -> False
         )
  )
  False
  (bundleMembers bundle)

generateBundle :: CompileContext -> Module -> IrBundle -> IO (Maybe TypePath)
generateBundle ctx mod bundle@(IrBundle name decls) = do
  -- if a bundle only contains type definitions, no further implementation
  -- is needed
  if bundleNeedsLib bundle
    then do
      generateBundleLib ctx name decls
      return $ Just name
    else return Nothing

generateBundleLib :: CompileContext -> TypePath -> [IrStmt] -> IO ()
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

generateDef :: CompileContext -> Handle -> IrStmt -> IO ()
generateDef ctx codeFile decl = do
  case stmt decl of
    FunctionDeclaration def@(FunctionDefinition { functionName = name, functionType = t, functionBody = Just body })
      -> do
        hPutStrLn codeFile ("\n" ++ (render $ pretty $ cfunDef def))

    VarDeclaration def@(VarDefinition { varName = name, varType = t, varDefault = val })
      -> do
        -- somewhat arbitrarily, compound literals aren't supported as static initializers in GCC <= 4
        hPutStrLn
          codeFile
          (  "\n"
          ++ (render $ pretty $ CDeclExt $ cDecl
               t
               (Just name)
               (case val of
                 Just val -> Just $ initializerExpr val
                 Nothing  -> Nothing
               )
             )
          )

    _ -> do
      return ()
