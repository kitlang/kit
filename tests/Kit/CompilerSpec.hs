module Kit.CompilerSpec where

import Control.Monad
import System.Directory
import System.FilePath
import System.Process
import Test.Hspec
import Test.QuickCheck
import Kit.Compiler
import Kit.Str

isKitFile :: FilePath -> Bool
isKitFile f = takeExtension f == ".kit"

readDir :: FilePath -> IO [FilePath]
readDir d = do
  isDir <- doesDirectoryExist d
  paths <- getDirectoryContents d
  files <- forM (paths) $ \d2 -> do
    isDir <- doesDirectoryExist (d </> d2)
    if (head d2) /= '_'
      then if (isDir && d2 /= "." && d2 /= "..")
        then readDir (d </> d2)
        else return (if isKitFile d2 then [(d </> d2)] else [])
      else return []
  return $ concat files

testFiles = readDir "tests/compile-src"

spec :: Spec
spec = parallel $ do
  describe "Successful compile tests" $ do
    paths <- runIO testFiles
    forM_ (paths) $ \path -> do
      it path $ do
        ctx    <- newCompileContext
        result <- tryCompile
          (ctx { ctxSourcePaths = [takeDirectory path, "std"]
               , ctxMainModule  = [s_pack $ takeFileName path -<.> ""]
               , ctxCompilerFlags = ["-Werror"]
               , ctxLinkerFlags  = ["-lm", "-Werror"]
               }
          )
        (case result of
            Left  err -> Just err
            Right ()  -> Nothing
          )
          `shouldBe` Nothing
        out               <- readProcess ("build" </> "main") [] ""
        outTemplateExists <- doesFileExist (path -<.> "stdout")
        when (outTemplateExists) $ do
          outTemplate <- readFile (path -<.> "stdout")
          out `shouldBe` outTemplate
