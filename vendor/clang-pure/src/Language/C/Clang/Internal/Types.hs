{-
Copyright 2014 Google Inc. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}

module Language.C.Clang.Internal.Types where

import Data.Singletons.TH
import Foreign

import Language.C.Clang.Internal.Refs

data CXIndexImpl
type CXIndex = Ptr CXIndexImpl
type instance RefOf ClangIndex = CXIndexImpl
newtype ClangIndex = ClangIndex (Root CXIndexImpl)
  deriving (Parent, Clang)

instance Eq ClangIndex where (==) = pointerEq

data CXTranslationUnitImpl
type CXTranslationUnit = Ptr CXTranslationUnitImpl
type instance RefOf TranslationUnit = CXTranslationUnitImpl
type instance ParentOf TranslationUnit = ClangIndex
newtype TranslationUnit = TranslationUnitRef (Node ClangIndex CXTranslationUnitImpl)
  deriving (Parent, Child, Clang)

instance Eq TranslationUnit where (==) = pointerEq

data CXCursor
type instance RefOf Cursor = CXCursor
type instance ParentOf Cursor = TranslationUnit
newtype Cursor = Cursor (Leaf TranslationUnit CXCursor)
  deriving (Child, Clang)

data CXSourceRange
type instance RefOf SourceRange = CXSourceRange
type instance ParentOf SourceRange = TranslationUnit
newtype SourceRange
  = SourceRange (Leaf TranslationUnit CXSourceRange)
  deriving (Child, Clang)

data CXSourceLocation
type instance RefOf SourceLocation = CXSourceLocation
type instance ParentOf SourceLocation = TranslationUnit
newtype SourceLocation
  = SourceLocation (Leaf TranslationUnit CXSourceLocation)
  deriving (Child, Clang)

data CXFileImpl
type CXFile = Ptr CXFileImpl
type instance RefOf File = CXFileImpl
type instance ParentOf File = TranslationUnit
newtype File
  = File (Leaf TranslationUnit CXFileImpl)
  deriving (Child, Clang)

instance Eq File where (==) = pointerEq

data CXString

data Location = Location
  { file :: File
  , line :: Word
  , column :: Word
  , offset :: Word
  }

data CursorKind
  = UnexposedDecl
  | StructDecl
  | UnionDecl
  | ClassDecl
  | EnumDecl
  | FieldDecl
  | EnumConstantDecl
  | FunctionDecl
  | VarDecl
  | ParmDecl
  | ObjCInterfaceDecl
  | ObjCCategoryDecl
  | ObjCProtocolDecl
  | ObjCPropertyDecl
  | ObjCIvarDecl
  | ObjCInstanceMethodDecl
  | ObjCClassMethodDecl
  | ObjCImplementationDecl
  | ObjCCategoryImplDecl
  | TypedefDecl
  | CXXMethod
  | Namespace
  | LinkageSpec
  | Constructor
  | Destructor
  | ConversionFunction
  | TemplateTypeParameter
  | NonTypeTemplateParameter
  | TemplateTemplateParameter
  | FunctionTemplate
  | ClassTemplate
  | ClassTemplatePartialSpecialization
  | NamespaceAlias
  | UsingDirective
  | UsingDeclaration
  | TypeAliasDecl
  | ObjCSynthesizeDecl
  | ObjCDynamicDecl
  | CXXAccessSpecifier
  | FirstDecl
  | LastDecl
  | FirstRef
  | ObjCSuperClassRef
  | ObjCProtocolRef
  | ObjCClassRef
  | TypeRef
  | CXXBaseSpecifier
  | TemplateRef
  | NamespaceRef
  | MemberRef
  | LabelRef
  | OverloadedDeclRef
  | VariableRef
  | LastRef
  | FirstInvalid
  | InvalidFile
  | NoDeclFound
  | NotImplemented
  | InvalidCode
  | LastInvalid
  | FirstExpr
  | UnexposedExpr
  | DeclRefExpr
  | MemberRefExpr
  | CallExpr
  | ObjCMessageExpr
  | BlockExpr
  | IntegerLiteral
  | FloatingLiteral
  | ImaginaryLiteral
  | StringLiteral
  | CharacterLiteral
  | ParenExpr
  | UnaryOperator
  | ArraySubscriptExpr
  | BinaryOperator
  | CompoundAssignOperator
  | ConditionalOperator
  | CStyleCastExpr
  | CompoundLiteralExpr
  | InitListExpr
  | AddrLabelExpr
  | StmtExpr
  | GenericSelectionExpr
  | GNUNullExpr
  | CXXStaticCastExpr
  | CXXDynamicCastExpr
  | CXXReinterpretCastExpr
  | CXXConstCastExpr
  | CXXFunctionalCastExpr
  | CXXTypeidExpr
  | CXXBoolLiteralExpr
  | CXXNullPtrLiteralExpr
  | CXXThisExpr
  | CXXThrowExpr
  | CXXNewExpr
  | CXXDeleteExpr
  | UnaryExpr
  | ObjCStringLiteral
  | ObjCEncodeExpr
  | ObjCSelectorExpr
  | ObjCProtocolExpr
  | ObjCBridgedCastExpr
  | PackExpansionExpr
  | SizeOfPackExpr
  | LambdaExpr
  | ObjCBoolLiteralExpr
  | ObjCSelfExpr
  | LastExpr
  | FirstStmt
  | UnexposedStmt
  | LabelStmt
  | CompoundStmt
  | CaseStmt
  | DefaultStmt
  | IfStmt
  | SwitchStmt
  | WhileStmt
  | DoStmt
  | ForStmt
  | GotoStmt
  | IndirectGotoStmt
  | ContinueStmt
  | BreakStmt
  | ReturnStmt
  | GCCAsmStmt
  | AsmStmt
  | ObjCAtTryStmt
  | ObjCAtCatchStmt
  | ObjCAtFinallyStmt
  | ObjCAtThrowStmt
  | ObjCAtSynchronizedStmt
  | ObjCAutoreleasePoolStmt
  | ObjCForCollectionStmt
  | CXXCatchStmt
  | CXXTryStmt
  | CXXForRangeStmt
  | SEHTryStmt
  | SEHExceptStmt
  | SEHFinallyStmt
  | MSAsmStmt
  | NullStmt
  | DeclStmt
  | OMPParallelDirective
  | OMPSimdDirective
  | OMPForDirective
  | OMPSectionsDirective
  | OMPSectionDirective
  | OMPSingleDirective
  | OMPParallelForDirective
  | OMPParallelSectionsDirective
  | OMPTaskDirective
  | OMPMasterDirective
  | OMPCriticalDirective
  | OMPTaskyieldDirective
  | OMPBarrierDirective
  | OMPTaskwaitDirective
  | OMPFlushDirective
  | SEHLeaveStmt
  | LastStmt
  | TranslationUnit
  | FirstAttr
  | UnexposedAttr
  | IBActionAttr
  | IBOutletAttr
  | IBOutletCollectionAttr
  | CXXFinalAttr
  | CXXOverrideAttr
  | AnnotateAttr
  | AsmLabelAttr
  | PackedAttr
  | PureAttr
  | ConstAttr
  | NoDuplicateAttr
  | CUDAConstantAttr
  | CUDADeviceAttr
  | CUDAGlobalAttr
  | CUDAHostAttr
  | LastAttr
  | PreprocessingDirective
  | MacroDefinition
  | MacroExpansion
  | MacroInstantiation
  | InclusionDirective
  | FirstPreprocessing
  | LastPreprocessing
  | ModuleImportDecl
  | FirstExtraDecl
  | LastExtraDecl
  deriving (Eq, Ord, Show)

genSingletons [''CursorKind]

data TypeKind
  = Invalid
  | Unexposed
  | Void
  | Bool
  | Char_U
  | UChar
  | Char16
  | Char32
  | UShort
  | UInt
  | ULong
  | ULongLong
  | UInt128
  | Char_S
  | SChar
  | WChar
  | Short
  | Int
  | Long
  | LongLong
  | Int128
  | Float
  | Double
  | LongDouble
  | NullPtr
  | Overload
  | Dependent
  | ObjCId
  | ObjCClass
  | ObjCSel
  | FirstBuiltin
  | LastBuiltin
  | Complex
  | Pointer
  | BlockPointer
  | LValueReference
  | RValueReference
  | Record
  | Enum
  | Typedef
  | ObjCInterface
  | ObjCObjectPointer
  | FunctionNoProto
  | FunctionProto
  | ConstantArray
  | Vector
  | IncompleteArray
  | VariableArray
  | DependentSizedArray
  | MemberPointer
    deriving (Eq, Ord, Show)

data CXType
type instance RefOf Type = CXType
type instance ParentOf Type = TranslationUnit
newtype Type = Type (Leaf TranslationUnit CXType)
  deriving (Child, Clang)

data CXToken
data TokenSet = TokenSet
  { tokenSetRef :: Leaf TranslationUnit CXToken
  , tokenSetSize :: Int
  }

type instance RefOf Token = CXToken
type instance ParentOf Token = TranslationUnit
data Token = Token TokenSet Int

data TypeLayoutError
  = TypeLayoutErrorInvalid
  | TypeLayoutErrorIncomplete
  | TypeLayoutErrorDependent
    deriving (Eq, Ord, Show)