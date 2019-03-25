{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

import           Language.C.Clang
import           Language.C.Clang.Cursor.Typed
import           Control.Lens
import qualified Data.ByteString.Char8 as BS
import           Data.Maybe
import           Data.Word
import           System.Environment

data CType =
  CArrayType  { cType :: Type
              , cCanonicalType :: Type
              , cElementType :: CType
              , cArraySize :: Word64, cSize :: Word64} |
  CScalarType { cType :: Type
              , cCanonicalType :: Type
              , cSize :: Word64
              } deriving (Show)

data CField = CField
  { cFieldName :: BS.ByteString
  , cFieldOffset :: Word64
  , cFieldType :: CType
  } deriving (Show)

data CStruct = CStruct
  { cStructName :: BS.ByteString
  , cStructFields :: [ CField ]
  } deriving (Show)

main :: IO ()
main = do
  args <- getArgs
  case args of
    path : clangArgs -> do
      idx <- createIndex
      tu <- parseTranslationUnit idx "clang" path clangArgs

      let toCType tp = let canonicalType = typeCanonicalType tp
                       in case typeKind canonicalType of
            ConstantArray -> do
              elementType <- toCType . fromJust . typeElementType $ canonicalType
              size <- typeSizeOf tp
              pure $ CArrayType
                { cType = tp
                , cCanonicalType = canonicalType
                , cElementType = elementType
                , cArraySize = fromJust (typeArraySize canonicalType)
                , cSize = size
                }
            _ -> do
              size <- typeSizeOf tp
              pure $ CScalarType
                { cType = tp
                , cCanonicalType = canonicalType
                , cSize = size
                }

      let toCField fieldDecC = do
            fieldOffset <- offsetOfField fieldDecC
            fieldType <- toCType (cursorType fieldDecC)
            return $ CField
              { cFieldName = cursorSpelling fieldDecC
              , cFieldOffset = fieldOffset
              , cFieldType = fieldType
              }

      let toCStruct structDecC = do
            let fieldDecs =
                  structDecC
                    ^.. cursorDescendantsF
                      . folding (matchKind @'FieldDecl)
            cFields <- traverse toCField fieldDecs
            return CStruct
              { cStructName = cursorSpelling structDecC
              , cStructFields = cFields
              }

      let cStructs =
            translationUnitCursor tu
              ^.. cursorDescendantsF
                . folding (matchKind @'StructDecl)
                . to toCStruct
                . _Right

      print cStructs

    _ -> putStrLn "usage: list-structs path [clang opts]"
