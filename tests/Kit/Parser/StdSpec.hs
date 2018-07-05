module Kit.Parser.StdSpec where

  import Control.Monad
  import System.Directory
  import System.FilePath
  import Test.Hspec
  import Test.QuickCheck
  import Kit.Error
  import Kit.Parser
  import Kit.Str

  isKitFile :: FilePath -> Bool
  isKitFile f = takeExtension f == ".kit"

  readDir :: FilePath -> IO [FilePath]
  readDir d = do
    isDir <- doesDirectoryExist d
    paths <- getDirectoryContents d
    files <- forM (paths) $ \d2 -> do
      isDir <- doesDirectoryExist (d </> d2)
      if (isDir && d2 /= "." && d2 /= "..")
        then readDir (d </> d2)
        else return (if isKitFile d2 then [(d </> d2)] else [])
    return $ concat files

  stdFiles = readDir "std"

  spec :: Spec
  spec = parallel $ do
    describe "Kit standard library" $ do
      paths <- runIO stdFiles
      forM_ (paths) $ \path -> do
        it path $ do
          result <- parseFile path
          case result of
            ParseResult x -> True `shouldBe` True
            Err e -> do logError e
                        error $ show e
