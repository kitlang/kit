module Kit.CompilerSpec where

import Control.Monad
import System.Directory
import System.Environment
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
sampleFiles = readDir "samples"

spec :: Spec
spec = parallel $ do
  describe "Successful compile tests" $ do
    testRunsArg <- runIO $ lookupEnv "TEST_RUNS"
    let testRuns = case testRunsArg of
          Just x  -> read x
          Nothing -> 1
    paths <- runIO testFiles
    forM_ (paths) $ \path -> do
      it path $ do
        forM_ [1 .. testRuns] $ \_ -> do
          ctx    <- newCompileContext
          cc     <- getCompiler Nothing
          result <- tryCompile
            (ctx
              { ctxSourcePaths = [ (f, []) | f <- [takeDirectory path, "std"] ]
              , ctxMainModule    = [s_pack $ takeFileName path -<.> ""]
              , ctxCompilerFlags = ["-Werror"]
              , ctxLinkerFlags   = ["-lm", "-Werror"]
              , ctxOutputPath    = ("build" </> "test")
              , ctxVerbose       = -1
              }
            )
            cc
          (case result of
              Left  err -> Just err
              Right ()  -> Nothing
            )
            `shouldBe` Nothing
          out <- readProcess ("build" </> "test") [] ""
          putStrLn out
          outTemplateExists <- doesFileExist (path -<.> "stdout")
          when (outTemplateExists) $ do
            outTemplate <- readFile (path -<.> "stdout")
            out `shouldBe` outTemplate


  describe "Build samples" $ do
    paths <- runIO sampleFiles
    forM_ (paths) $ \path -> do
      it path $ do
        ctx    <- newCompileContext
        cc     <- getCompiler Nothing
        result <- tryCompile
          (ctx { ctxSourcePaths = [ (f, []) | f <- [takeDirectory path, "std"] ]
               , ctxMainModule    = [s_pack $ takeFileName path -<.> ""]
               , ctxCompilerFlags = ["-Werror"]
               , ctxLinkerFlags   = ["-lm", "-Werror"]
               , ctxOutputPath    = ("build" </> "test")
               , ctxVerbose       = -1
               }
          )
          cc
        (case result of
            Left  err -> Just err
            Right ()  -> Nothing
          )
          `shouldBe` Nothing

  describe "Test decideModuleAndSourcePaths" $ do
    let testData =
          [ -- We default to src if we have no source paths and a file wasn't specified directly
            ("main"    , [], ("main", ["src"]))
          , ("main.kit", [], ("main", ["."]))
          , ( "src/main.kit"
            , []
            , ("main", ["src"])
            )

                     -- To make sure we don't generate redundant entries in case of paths that need normalization.
                     -- Also that we normalize paths.
          , ("./src/main.kit"  , []        , ("main", ["src"]))
          , ("src/main.kit"    , ["./src/"], ("main", ["src"]))
          , ("src/pkg/main.kit", []        , ("main", ["src/pkg"]))
          , ("src/pkg/main.kit", ["src"]   , ("pkg.main", ["src"]))
          ]
    forM_ (testData) $ \d -> do
      let (mod, paths, result) = d
      it (show (mod, paths)) $ do
        decideModuleAndSourcePaths mod paths `shouldBe` result
