{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

{-|
Module      :  Language.C.Clang.Cursor.Typed
Copyright   :  (C) 2016 Patrick Chilton

This module contains a typed version of "Language.C.Clang.Cursor".
Here, we keep track of `CursorKind`s at type-level, which means
that you don't need to check whether a `Cursor` has a given property at runtime.
-}
module Language.C.Clang.Cursor.Typed
  ( Cursor
  , CursorK()
  , withoutKind
  , matchKind

  , translationUnitCursor

  , cursorType

  , cursorChildrenF
  , cursorChildren
  , cursorDescendantsF
  , cursorDescendants

  , cursorExtent

  , cursorSpelling

  , TypeLayoutError(..)
  , offsetOfField

  , HasType
  , HasChildren
  , HasExtent
  , HasSpelling
  , CursorKind(..)
  ) where

import qualified Data.ByteString as BS
import           Data.Functor.Contravariant
import           Data.Maybe
import           Data.Singletons
import           Data.Word
import           Lens.Micro.Contra

import qualified Language.C.Clang.Cursor as UT
import           Language.C.Clang.Cursor ( cursorKind, Cursor, CursorKind(..) )
import           Language.C.Clang.Location
import           Language.C.Clang.Internal.Types

import           Language.C.Clang.Internal.Refs (Clang)

-- | A `Cursor` with a statically known `CursorKind`.
newtype CursorK (kind :: CursorKind) = CursorK { withoutKind :: Cursor }
  deriving (Eq, Clang, Show)

-- | Match a `Cursor` as a particular `CursorKind`. You can use the @TypeApplications@ extension to easily specify the `CursorKind` you want: @matchKind \@'StructDecl@.
matchKind :: forall kind. SingI kind => Cursor -> Maybe (CursorK kind)
matchKind c
  | cursorKind c == fromSing (sing :: Sing kind) = Just (CursorK c)
  | otherwise = Nothing

translationUnitCursor :: TranslationUnit -> CursorK 'TranslationUnit
translationUnitCursor = CursorK . UT.translationUnitCursor

class HasType (kind :: CursorKind)
cursorType :: HasType kind => CursorK kind -> Type
cursorType = fromJust . UT.cursorType . withoutKind

class HasChildren (kind :: CursorKind)

to :: (s -> a) -> Getter s a
to k f = phantom . f . k

cursorChildrenF :: HasChildren kind => Fold (CursorK kind) Cursor
cursorChildrenF = to withoutKind . UT.cursorChildrenF

cursorChildren :: HasChildren kind => CursorK kind -> [ Cursor ]
cursorChildren = UT.cursorChildren . withoutKind

-- | `Fold` over a `CursorK` and all of its descendants recursively.
cursorDescendantsF :: HasChildren kind => Fold (CursorK kind) Cursor
cursorDescendantsF = to withoutKind . UT.cursorDescendantsF

-- | List a `CursorK` and all of its descendants recursively.
cursorDescendants :: HasChildren kind => CursorK kind -> [ Cursor ]
cursorDescendants = UT.cursorDescendants . withoutKind

class HasExtent (kind :: CursorKind)

cursorExtent :: HasExtent kind => CursorK kind -> SourceRange
cursorExtent = fromJust . UT.cursorExtent . withoutKind

class HasSpelling (kind :: CursorKind)

cursorSpelling :: HasSpelling kind => CursorK kind -> BS.ByteString
cursorSpelling = UT.cursorSpelling . withoutKind

offsetOfField :: CursorK 'FieldDecl -> Either TypeLayoutError Word64
offsetOfField = UT.offsetOfField . withoutKind

-- instances derived experimentally with the find-classes executable
instance HasChildren 'ArraySubscriptExpr
instance HasChildren 'BinaryOperator
instance HasChildren 'CStyleCastExpr
instance HasChildren 'CXXBaseSpecifier
instance HasChildren 'CXXCatchStmt
instance HasChildren 'CXXConstCastExpr
instance HasChildren 'CXXDeleteExpr
instance HasChildren 'CXXDynamicCastExpr
instance HasChildren 'CXXFunctionalCastExpr
instance HasChildren 'CXXMethod
instance HasChildren 'CXXNewExpr
instance HasChildren 'CXXReinterpretCastExpr
instance HasChildren 'CXXStaticCastExpr
instance HasChildren 'CXXTryStmt
instance HasChildren 'CallExpr
instance HasChildren 'CaseStmt
instance HasChildren 'ClassDecl
instance HasChildren 'ClassTemplate
instance HasChildren 'ClassTemplatePartialSpecialization
instance HasChildren 'CompoundAssignOperator
instance HasChildren 'CompoundStmt
instance HasChildren 'ConditionalOperator
instance HasChildren 'Constructor
instance HasChildren 'ConversionFunction
instance HasChildren 'DeclRefExpr
instance HasChildren 'DeclStmt
instance HasChildren 'DefaultStmt
instance HasChildren 'Destructor
instance HasChildren 'DoStmt
instance HasChildren 'EnumConstantDecl
instance HasChildren 'EnumDecl
instance HasChildren 'FieldDecl
instance HasChildren 'FirstExpr
instance HasChildren 'ForStmt
instance HasChildren 'FunctionDecl
instance HasChildren 'FunctionTemplate
instance HasChildren 'IfStmt
instance HasChildren 'InitListExpr
instance HasChildren 'MemberRefExpr
instance HasChildren 'Namespace
instance HasChildren 'NonTypeTemplateParameter
instance HasChildren 'ParenExpr
instance HasChildren 'ParmDecl
instance HasChildren 'ReturnStmt
instance HasChildren 'StructDecl
instance HasChildren 'SwitchStmt
instance HasChildren 'TemplateTypeParameter
instance HasChildren 'TranslationUnit
instance HasChildren 'TypedefDecl
instance HasChildren 'UnaryOperator
instance HasChildren 'UnexposedDecl
instance HasChildren 'UnionDecl
instance HasChildren 'UsingDeclaration
instance HasChildren 'UsingDirective
instance HasChildren 'VarDecl
instance HasChildren 'WhileStmt

instance HasType 'ArraySubscriptExpr
instance HasType 'BinaryOperator
instance HasType 'CStyleCastExpr
instance HasType 'CXXBaseSpecifier
instance HasType 'CXXBoolLiteralExpr
instance HasType 'CXXConstCastExpr
instance HasType 'CXXDeleteExpr
instance HasType 'CXXDynamicCastExpr
instance HasType 'CXXFunctionalCastExpr
instance HasType 'CXXMethod
instance HasType 'CXXNewExpr
instance HasType 'CXXReinterpretCastExpr
instance HasType 'CXXStaticCastExpr
instance HasType 'CXXThisExpr
instance HasType 'CXXThrowExpr
instance HasType 'CallExpr
instance HasType 'CharacterLiteral
instance HasType 'ClassDecl
instance HasType 'ClassTemplatePartialSpecialization
instance HasType 'CompoundAssignOperator
instance HasType 'ConditionalOperator
instance HasType 'Constructor
instance HasType 'ConversionFunction
instance HasType 'DeclRefExpr
instance HasType 'Destructor
instance HasType 'EnumConstantDecl
instance HasType 'EnumDecl
instance HasType 'FieldDecl
instance HasType 'FloatingLiteral
instance HasType 'FunctionDecl
instance HasType 'FunctionTemplate
instance HasType 'GNUNullExpr
instance HasType 'InitListExpr
instance HasType 'IntegerLiteral
instance HasType 'MemberRef
instance HasType 'MemberRefExpr
instance HasType 'NonTypeTemplateParameter
instance HasType 'ParenExpr
instance HasType 'ParmDecl
instance HasType 'StringLiteral
instance HasType 'StructDecl
instance HasType 'TemplateTypeParameter
instance HasType 'TypeRef
instance HasType 'TypedefDecl
instance HasType 'UnaryOperator
instance HasType 'UnionDecl
instance HasType 'VarDecl

instance HasExtent 'ArraySubscriptExpr
instance HasExtent 'AsmLabelAttr
instance HasExtent 'BinaryOperator
instance HasExtent 'BreakStmt
instance HasExtent 'CStyleCastExpr
instance HasExtent 'CXXAccessSpecifier
instance HasExtent 'CXXBaseSpecifier
instance HasExtent 'CXXBoolLiteralExpr
instance HasExtent 'CXXCatchStmt
instance HasExtent 'CXXConstCastExpr
instance HasExtent 'CXXDeleteExpr
instance HasExtent 'CXXDynamicCastExpr
instance HasExtent 'CXXFunctionalCastExpr
instance HasExtent 'CXXMethod
instance HasExtent 'CXXNewExpr
instance HasExtent 'CXXReinterpretCastExpr
instance HasExtent 'CXXStaticCastExpr
instance HasExtent 'CXXThisExpr
instance HasExtent 'CXXThrowExpr
instance HasExtent 'CXXTryStmt
instance HasExtent 'CallExpr
instance HasExtent 'CaseStmt
instance HasExtent 'CharacterLiteral
instance HasExtent 'ClassDecl
instance HasExtent 'ClassTemplate
instance HasExtent 'ClassTemplatePartialSpecialization
instance HasExtent 'CompoundAssignOperator
instance HasExtent 'CompoundStmt
instance HasExtent 'ConditionalOperator
instance HasExtent 'ConstAttr
instance HasExtent 'Constructor
instance HasExtent 'ContinueStmt
instance HasExtent 'ConversionFunction
instance HasExtent 'DeclRefExpr
instance HasExtent 'DeclStmt
instance HasExtent 'DefaultStmt
instance HasExtent 'Destructor
instance HasExtent 'DoStmt
instance HasExtent 'EnumConstantDecl
instance HasExtent 'EnumDecl
instance HasExtent 'FloatingLiteral
instance HasExtent 'ForStmt
instance HasExtent 'FunctionDecl
instance HasExtent 'FunctionTemplate
instance HasExtent 'GNUNullExpr
instance HasExtent 'IfStmt
instance HasExtent 'InitListExpr
instance HasExtent 'MemberRef
instance HasExtent 'MemberRefExpr
instance HasExtent 'Namespace
instance HasExtent 'NamespaceRef
instance HasExtent 'NonTypeTemplateParameter
instance HasExtent 'NullStmt
instance HasExtent 'OverloadedDeclRef
instance HasExtent 'ParenExpr
instance HasExtent 'PureAttr
instance HasExtent 'ReturnStmt
instance HasExtent 'StringLiteral
instance HasExtent 'StructDecl
instance HasExtent 'SwitchStmt
instance HasExtent 'TemplateRef
instance HasExtent 'TemplateTypeParameter
instance HasExtent 'TranslationUnit
instance HasExtent 'TypeRef
instance HasExtent 'TypedefDecl
instance HasExtent 'UnaryOperator
instance HasExtent 'UnionDecl
instance HasExtent 'UsingDeclaration
instance HasExtent 'UsingDirective
instance HasExtent 'WhileStmt

instance HasSpelling 'AsmLabelAttr
instance HasSpelling 'CXXBaseSpecifier
instance HasSpelling 'CXXMethod
instance HasSpelling 'ClassDecl
instance HasSpelling 'ClassTemplate
instance HasSpelling 'ClassTemplatePartialSpecialization
instance HasSpelling 'Constructor
instance HasSpelling 'ConversionFunction
instance HasSpelling 'Destructor
instance HasSpelling 'EnumConstantDecl
instance HasSpelling 'FieldDecl
instance HasSpelling 'FunctionDecl
instance HasSpelling 'FunctionTemplate
instance HasSpelling 'MemberRef
instance HasSpelling 'Namespace
instance HasSpelling 'NamespaceRef
instance HasSpelling 'OverloadedDeclRef
instance HasSpelling 'StringLiteral
instance HasSpelling 'StructDecl
instance HasSpelling 'TemplateRef
instance HasSpelling 'TranslationUnit
instance HasSpelling 'TypeRef
instance HasSpelling 'TypedefDecl
instance HasSpelling 'UsingDeclaration