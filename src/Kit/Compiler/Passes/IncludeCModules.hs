module Kit.Compiler.Passes.IncludeCModules where

  import Control.Exception
  import Control.Monad
  import Data.IORef
  import Data.List
  import System.Directory
  import System.FilePath
  import Language.C
  import Language.C.System.GCC
  import Kit.Ast
  import Kit.Compiler.Context
  import Kit.Compiler.Module
  import Kit.Compiler.Utils
  import Kit.Error
  import Kit.HashTable
  import Kit.Parser

  includeCModules :: CompileContext -> IO ()
  includeCModules ctx = do
    includes <- readIORef (context_includes ctx)
    forM includes (includeCHeader ctx)
    return ()

  includeCHeader :: CompileContext -> FilePath -> IO Module
  includeCHeader ctx path = do
    existing <- h_lookup (context_cmodules ctx) path
    case existing of
      Just x -> do return x
      Nothing -> do
        debugLog ctx $ "searching for header " ++ show path
        found <- findSourceFile path (context_include_paths ctx)
        case found of
          Just f -> do
            mod <- parseCHeader ctx f
            h_insert (context_cmodules ctx) path mod
            return mod
          Nothing -> throw $ Errs $ [
              err IncludeError ("Couldn't find header " ++ show path ++
                                "; tried searching the following locations: \n\n" ++
                                (intercalate "\n" ["  - " ++ (dir </> path) | dir <- context_include_paths ctx]))
            ]

  parseCHeader :: CompileContext -> FilePath -> IO Module
  parseCHeader ctx path = do
    putStrLn path
    parseResult <- parseCFile (newGCC "gcc") Nothing [] path
    case parseResult of
      Left e -> throw $ Errs [err IncludeError ("Parsing C header " ++ show path ++ " failed: " ++ show e)]
      Right (CTranslUnit decls _) -> do
        mod <- newCMod []
        parseCDecls ctx mod decls
        return mod

  parseCDecls :: CompileContext -> Module -> [CExtDecl] -> IO ()
  parseCDecls ctx mod [] = do return ()
  parseCDecls ctx mod (h:t) = do
    case h of
      CDeclExt d -> do
        -- TODO
        --putStrLn $ show h
        return ()
      {-CFDefExt f -> do
        -- TODO
        putStrLn $ show h-}
      _ -> do return ()
    parseCDecls ctx mod t
