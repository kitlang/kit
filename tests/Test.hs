module Main where

  import Test.Hspec.Runner
  import Test.Hspec.Formatters
  import qualified Spec

  main :: IO ()
  main = hspec Spec.spec
