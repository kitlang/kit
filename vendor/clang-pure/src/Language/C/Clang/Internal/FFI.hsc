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

{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-overlapping-patterns #-}

module Language.C.Clang.Internal.FFI where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Control.Exception
import Control.Monad
import Data.Foldable
import Data.Functor.Contravariant
import Data.IORef
import qualified Data.Vector.Storable as VS
import Foreign
import Foreign.C
import qualified Language.C.Inline as C hiding (exp, block)
import qualified Language.C.Inline as CSafe
import qualified Language.C.Inline.Unsafe as C
import System.IO.Unsafe
import Lens.Micro.Contra

import Language.C.Clang.Internal.Context
import Language.C.Clang.Internal.Refs
import Language.C.Clang.Internal.Types

C.context clangCtx
C.include "stdlib.h"
#include  "clang-c/Index.h"
C.include "clang-c/Index.h"
C.include "clang-pure-utils.h"

foreign import ccall "clang_disposeIndex"
  clang_disposeIndex :: Ptr CXIndexImpl -> Finalizer

createIndex :: IO ClangIndex
createIndex = do
  idxp <- [C.exp| CXIndex { clang_createIndex(0, 1) } |]
  ClangIndex <$> newRoot idxp (clang_disposeIndex idxp)

foreign import ccall "clang_disposeTranslationUnit"
  clang_disposeTranslationUnit :: Ptr CXTranslationUnitImpl -> Finalizer

data ClangError
  = Success
  | Failure
  | Crashed
  | InvalidArguments
  | ASTReadError
  deriving (Eq, Ord, Show)

parseClangError :: CInt -> ClangError
parseClangError = \case
  #{const CXError_Success} -> Success
  #{const CXError_Failure} -> Failure
  #{const CXError_Crashed} -> Crashed
  #{const CXError_InvalidArguments} -> InvalidArguments
  #{const CXError_ASTReadError} -> ASTReadError
  _ -> Failure -- unrecognized enum value

instance Exception ClangError

parseTranslationUnit :: ClangIndex -> FilePath -> FilePath -> [ String ] -> IO TranslationUnit
parseTranslationUnit idx cc path args = do
  tun <- newNode idx $ \idxp ->
    withCString path $ \cPath -> do
      ( tup, cres ) <- bracket
        (traverse newCString (cc : args))
        (traverse_ free) $
        \cArgList -> do
          let cArgs = VS.fromList cArgList
          C.withPtr $ \tupp -> [C.exp| int {
            clang_parseTranslationUnit2FullArgv(
              $(CXIndex idxp),
              $(char* cPath),
              $vec-ptr:(const char * const * cArgs), $vec-len:cArgs,
              NULL, 0,
              CXTranslationUnit_DetailedPreprocessingRecord,
              $(CXTranslationUnit *tupp))
            } |]
      let res = parseClangError cres
      when (res /= Success) $ throwIO res
      return ( tup, clang_disposeTranslationUnit tup )
  return $ TranslationUnitRef tun

translationUnitCursor :: TranslationUnit -> Cursor
translationUnitCursor tu = unsafePerformIO $ do
  cn <- newLeaf tu $ \tup -> do
    cp <- [C.exp| CXCursor* { ALLOC(
      clang_getTranslationUnitCursor($(CXTranslationUnit tup))
      )} |]
    return ( cp, free cp )
  return $ Cursor cn

cursorTranslationUnit :: Cursor -> TranslationUnit
cursorTranslationUnit (Cursor c) = parent c

cursorKind :: Cursor -> CursorKind
cursorKind c = uderef c $ fmap parseCursorKind . #peek CXCursor, kind

-- | Fold over the children of a cursor in the `lens` sense.
cursorChildrenF :: Fold Cursor Cursor
cursorChildrenF f c = uderef c $ \cp -> do
  -- initialize the "Fold state" with no effect
  fRef <- newIORef $ phantom $ pure ()
  let
    visitChild chp = do
      ch <- newLeaf (cursorTranslationUnit c) $ \_ ->
        return ( chp, free chp )
        -- fold over the new cursor and update the "Fold state"
      modifyIORef' fRef (*> f (Cursor ch))
  [CSafe.exp| void {
    clang_visitChildren(
      *$(CXCursor *cp),
      visit_haskell,
      $fun:(void (*visitChild)(CXCursor*)))
    } |]
  readIORef fRef

withCXString :: (Ptr CXString -> IO ()) -> IO ByteString
withCXString f = allocaBytes (#size CXString) $ \cxsp -> do
  f cxsp
  bracket
    [C.exp| const char * { clang_getCString(*$(CXString *cxsp)) } |]
    (\_ -> [C.exp| void { clang_disposeString(*$(CXString *cxsp)) } |]) $
    \cs -> BS.packCString cs

cursorSpelling :: Cursor -> ByteString
cursorSpelling c = uderef c $ \cp -> withCXString $ \cxsp ->
  [C.block| void {
    *$(CXString *cxsp) = clang_getCursorSpelling(*$(CXCursor *cp));
    } |]

cursorExtent :: Cursor -> Maybe SourceRange
cursorExtent c = uderef c $ \cp -> do
  srp <- [C.block| CXSourceRange* {
    CXSourceRange sr = clang_getCursorExtent(*$(CXCursor *cp));
    if (clang_Range_isNull(sr)) {
      return NULL;
    }

    return ALLOC(sr);
    } |]
  if srp == nullPtr
    then return Nothing
    else do
      srn <- newLeaf (cursorTranslationUnit c) $ \_ ->
        return ( srp, free srp )
      return $ Just $ SourceRange srn

cursorUSR :: Cursor -> ByteString
cursorUSR c = uderef c $ \cp -> withCXString $ \cxsp ->
  [C.block| void {
    *$(CXString *cxsp) = clang_getCursorUSR(*$(CXCursor *cp));
    } |]

cursorReferenced :: Cursor -> Maybe Cursor
cursorReferenced c = uderef c $ \cp -> do
  rcp <- [C.block| CXCursor* {
    CXCursor ref = clang_getCursorReferenced(*$(CXCursor *cp));
    if (clang_Cursor_isNull(ref)) {
      return NULL;
    }

    return ALLOC(ref);
    } |]
  if rcp /= nullPtr
    then (Just . Cursor) <$> newLeaf (parent c) (\_ -> return ( rcp, free rcp ))
    else return Nothing

cursorDefinition :: Cursor -> Maybe Cursor
cursorDefinition c = uderef c $ \cp -> do
  rcp <- [C.block| CXCursor* {
    CXCursor ref = clang_getCursorDefinition(*$(CXCursor *cp));
    if (clang_Cursor_isNull(ref)) {
      return NULL;
    }

    return ALLOC(ref);
    } |]
  if rcp /= nullPtr
    then (Just . Cursor) <$> newLeaf (parent c) (\_ -> return ( rcp, free rcp ))
    else return Nothing

rangeStart, rangeEnd :: SourceRange -> SourceLocation
rangeStart sr = uderef sr $ \srp -> do
  slp <- [C.exp| CXSourceLocation* { ALLOC(
    clang_getRangeStart(*$(CXSourceRange *srp))
    )} |]
  sln <- newLeaf (parent sr) $ \_ ->
    return ( slp, free slp )
  return $ SourceLocation sln

rangeEnd sr = uderef sr $ \srp -> do
  slp <- [C.exp| CXSourceLocation* { ALLOC(
    clang_getRangeEnd(*$(CXSourceRange *srp))
    )} |]
  sln <- newLeaf (parent sr) $ \_ ->
    return ( slp, free slp )
  return $ SourceLocation sln

spellingLocation :: SourceLocation -> Location
spellingLocation sl = uderef sl $ \slp -> do
  ( f, l, c, o ) <- C.withPtrs_ $ \( fp, lp, cp, offp ) ->
    [C.exp| void {
      clang_getSpellingLocation(
        *$(CXSourceLocation *slp),
        $(CXFile *fp),
        $(unsigned int *lp),
        $(unsigned int *cp),
        $(unsigned int *offp))
      } |]
  fn <- newLeaf (parent sl) $ \_ -> return ( f, return () )
  return $ Location
    { file = File fn
    , line = fromIntegral l
    , column = fromIntegral c
    , offset = fromIntegral o
    }

getFile :: TranslationUnit -> FilePath -> Maybe File
getFile tu p = uderef tu $ \tup -> withCString p $ \fn -> do
  fp <- [C.exp| CXFile {
    clang_getFile($(CXTranslationUnit tup), $(const char *fn))
    } |]
  if fp == nullPtr
    then return Nothing
    else (Just . File) <$> newLeaf tu (\_ -> return ( fp, return () ))

fileName :: File -> ByteString
fileName f = uderef f $ \fp -> withCXString $ \cxsp ->
  [C.block| void {
    *$(CXString *cxsp) = clang_getFileName($(CXFile fp));
    } |]

instance Eq Cursor where
  (==) = defaultEq $ \lp rp ->
    [C.exp| int { clang_equalCursors(*$(CXCursor *lp), *$(CXCursor *rp)) } |]

instance Eq SourceRange where
  (==) = defaultEq $ \lp rp ->
    [C.exp| int { clang_equalRanges(*$(CXSourceRange *lp), *$(CXSourceRange *rp)) } |]

instance Eq SourceLocation where
  (==) = defaultEq $ \lp rp ->
    [C.exp| int { clang_equalLocations(*$(CXSourceLocation *lp), *$(CXSourceLocation *rp)) } |]

instance Eq Type where
  (==) = defaultEq $ \lp rp ->
    [C.exp| int { clang_equalTypes(*$(CXType *lp), *$(CXType *rp)) } |]

-- Checks for pointer equality, alternatively checks for structural equality with the given function.
defaultEq :: (Clang r, RefOf r ~ a) => (Ptr a -> Ptr a -> IO CInt) -> r -> r -> Bool
defaultEq ne l r
  = l `pointerEq` r || structEq
    where
      structEq =
        unsafePerformIO $
          deref l $ \p ->
            deref r $ \p' ->
              (/=0) <$> ne p p'

parseCursorKind :: CInt -> CursorKind
parseCursorKind = \case
  #{const CXCursor_UnexposedDecl} -> UnexposedDecl
  #{const CXCursor_StructDecl} -> StructDecl
  #{const CXCursor_UnionDecl} -> UnionDecl
  #{const CXCursor_ClassDecl} -> ClassDecl
  #{const CXCursor_EnumDecl} -> EnumDecl
  #{const CXCursor_FieldDecl} -> FieldDecl
  #{const CXCursor_EnumConstantDecl} -> EnumConstantDecl
  #{const CXCursor_FunctionDecl} -> FunctionDecl
  #{const CXCursor_VarDecl} -> VarDecl
  #{const CXCursor_ParmDecl} -> ParmDecl
  #{const CXCursor_ObjCInterfaceDecl} -> ObjCInterfaceDecl
  #{const CXCursor_ObjCCategoryDecl} -> ObjCCategoryDecl
  #{const CXCursor_ObjCProtocolDecl} -> ObjCProtocolDecl
  #{const CXCursor_ObjCPropertyDecl} -> ObjCPropertyDecl
  #{const CXCursor_ObjCIvarDecl} -> ObjCIvarDecl
  #{const CXCursor_ObjCInstanceMethodDecl} -> ObjCInstanceMethodDecl
  #{const CXCursor_ObjCClassMethodDecl} -> ObjCClassMethodDecl
  #{const CXCursor_ObjCImplementationDecl} -> ObjCImplementationDecl
  #{const CXCursor_ObjCCategoryImplDecl} -> ObjCCategoryImplDecl
  #{const CXCursor_TypedefDecl} -> TypedefDecl
  #{const CXCursor_CXXMethod} -> CXXMethod
  #{const CXCursor_Namespace} -> Namespace
  #{const CXCursor_LinkageSpec} -> LinkageSpec
  #{const CXCursor_Constructor} -> Constructor
  #{const CXCursor_Destructor} -> Destructor
  #{const CXCursor_ConversionFunction} -> ConversionFunction
  #{const CXCursor_TemplateTypeParameter} -> TemplateTypeParameter
  #{const CXCursor_NonTypeTemplateParameter} -> NonTypeTemplateParameter
  #{const CXCursor_TemplateTemplateParameter} -> TemplateTemplateParameter
  #{const CXCursor_FunctionTemplate} -> FunctionTemplate
  #{const CXCursor_ClassTemplate} -> ClassTemplate
  #{const CXCursor_ClassTemplatePartialSpecialization} -> ClassTemplatePartialSpecialization
  #{const CXCursor_NamespaceAlias} -> NamespaceAlias
  #{const CXCursor_UsingDirective} -> UsingDirective
  #{const CXCursor_UsingDeclaration} -> UsingDeclaration
  #{const CXCursor_TypeAliasDecl} -> TypeAliasDecl
  #{const CXCursor_ObjCSynthesizeDecl} -> ObjCSynthesizeDecl
  #{const CXCursor_ObjCDynamicDecl} -> ObjCDynamicDecl
  #{const CXCursor_CXXAccessSpecifier} -> CXXAccessSpecifier
  #{const CXCursor_FirstDecl} -> FirstDecl
  #{const CXCursor_LastDecl} -> LastDecl
  #{const CXCursor_FirstRef} -> FirstRef
  #{const CXCursor_ObjCSuperClassRef} -> ObjCSuperClassRef
  #{const CXCursor_ObjCProtocolRef} -> ObjCProtocolRef
  #{const CXCursor_ObjCClassRef} -> ObjCClassRef
  #{const CXCursor_TypeRef} -> TypeRef
  #{const CXCursor_CXXBaseSpecifier} -> CXXBaseSpecifier
  #{const CXCursor_TemplateRef} -> TemplateRef
  #{const CXCursor_NamespaceRef} -> NamespaceRef
  #{const CXCursor_MemberRef} -> MemberRef
  #{const CXCursor_LabelRef} -> LabelRef
  #{const CXCursor_OverloadedDeclRef} -> OverloadedDeclRef
  #{const CXCursor_VariableRef} -> VariableRef
  #{const CXCursor_LastRef} -> LastRef
  #{const CXCursor_FirstInvalid} -> FirstInvalid
  #{const CXCursor_InvalidFile} -> InvalidFile
  #{const CXCursor_NoDeclFound} -> NoDeclFound
  #{const CXCursor_NotImplemented} -> NotImplemented
  #{const CXCursor_InvalidCode} -> InvalidCode
  #{const CXCursor_LastInvalid} -> LastInvalid
  #{const CXCursor_FirstExpr} -> FirstExpr
  #{const CXCursor_UnexposedExpr} -> UnexposedExpr
  #{const CXCursor_DeclRefExpr} -> DeclRefExpr
  #{const CXCursor_MemberRefExpr} -> MemberRefExpr
  #{const CXCursor_CallExpr} -> CallExpr
  #{const CXCursor_ObjCMessageExpr} -> ObjCMessageExpr
  #{const CXCursor_BlockExpr} -> BlockExpr
  #{const CXCursor_IntegerLiteral} -> IntegerLiteral
  #{const CXCursor_FloatingLiteral} -> FloatingLiteral
  #{const CXCursor_ImaginaryLiteral} -> ImaginaryLiteral
  #{const CXCursor_StringLiteral} -> StringLiteral
  #{const CXCursor_CharacterLiteral} -> CharacterLiteral
  #{const CXCursor_ParenExpr} -> ParenExpr
  #{const CXCursor_UnaryOperator} -> UnaryOperator
  #{const CXCursor_ArraySubscriptExpr} -> ArraySubscriptExpr
  #{const CXCursor_BinaryOperator} -> BinaryOperator
  #{const CXCursor_CompoundAssignOperator} -> CompoundAssignOperator
  #{const CXCursor_ConditionalOperator} -> ConditionalOperator
  #{const CXCursor_CStyleCastExpr} -> CStyleCastExpr
  #{const CXCursor_CompoundLiteralExpr} -> CompoundLiteralExpr
  #{const CXCursor_InitListExpr} -> InitListExpr
  #{const CXCursor_AddrLabelExpr} -> AddrLabelExpr
  #{const CXCursor_StmtExpr} -> StmtExpr
  #{const CXCursor_GenericSelectionExpr} -> GenericSelectionExpr
  #{const CXCursor_GNUNullExpr} -> GNUNullExpr
  #{const CXCursor_CXXStaticCastExpr} -> CXXStaticCastExpr
  #{const CXCursor_CXXDynamicCastExpr} -> CXXDynamicCastExpr
  #{const CXCursor_CXXReinterpretCastExpr} -> CXXReinterpretCastExpr
  #{const CXCursor_CXXConstCastExpr} -> CXXConstCastExpr
  #{const CXCursor_CXXFunctionalCastExpr} -> CXXFunctionalCastExpr
  #{const CXCursor_CXXTypeidExpr} -> CXXTypeidExpr
  #{const CXCursor_CXXBoolLiteralExpr} -> CXXBoolLiteralExpr
  #{const CXCursor_CXXNullPtrLiteralExpr} -> CXXNullPtrLiteralExpr
  #{const CXCursor_CXXThisExpr} -> CXXThisExpr
  #{const CXCursor_CXXThrowExpr} -> CXXThrowExpr
  #{const CXCursor_CXXNewExpr} -> CXXNewExpr
  #{const CXCursor_CXXDeleteExpr} -> CXXDeleteExpr
  #{const CXCursor_UnaryExpr} -> UnaryExpr
  #{const CXCursor_ObjCStringLiteral} -> ObjCStringLiteral
  #{const CXCursor_ObjCEncodeExpr} -> ObjCEncodeExpr
  #{const CXCursor_ObjCSelectorExpr} -> ObjCSelectorExpr
  #{const CXCursor_ObjCProtocolExpr} -> ObjCProtocolExpr
  #{const CXCursor_ObjCBridgedCastExpr} -> ObjCBridgedCastExpr
  #{const CXCursor_PackExpansionExpr} -> PackExpansionExpr
  #{const CXCursor_SizeOfPackExpr} -> SizeOfPackExpr
  #{const CXCursor_LambdaExpr} -> LambdaExpr
  #{const CXCursor_ObjCBoolLiteralExpr} -> ObjCBoolLiteralExpr
  #{const CXCursor_ObjCSelfExpr} -> ObjCSelfExpr
  #{const CXCursor_LastExpr} -> LastExpr
  #{const CXCursor_FirstStmt} -> FirstStmt
  #{const CXCursor_UnexposedStmt} -> UnexposedStmt
  #{const CXCursor_LabelStmt} -> LabelStmt
  #{const CXCursor_CompoundStmt} -> CompoundStmt
  #{const CXCursor_CaseStmt} -> CaseStmt
  #{const CXCursor_DefaultStmt} -> DefaultStmt
  #{const CXCursor_IfStmt} -> IfStmt
  #{const CXCursor_SwitchStmt} -> SwitchStmt
  #{const CXCursor_WhileStmt} -> WhileStmt
  #{const CXCursor_DoStmt} -> DoStmt
  #{const CXCursor_ForStmt} -> ForStmt
  #{const CXCursor_GotoStmt} -> GotoStmt
  #{const CXCursor_IndirectGotoStmt} -> IndirectGotoStmt
  #{const CXCursor_ContinueStmt} -> ContinueStmt
  #{const CXCursor_BreakStmt} -> BreakStmt
  #{const CXCursor_ReturnStmt} -> ReturnStmt
  #{const CXCursor_GCCAsmStmt} -> GCCAsmStmt
  #{const CXCursor_AsmStmt} -> AsmStmt
  #{const CXCursor_ObjCAtTryStmt} -> ObjCAtTryStmt
  #{const CXCursor_ObjCAtCatchStmt} -> ObjCAtCatchStmt
  #{const CXCursor_ObjCAtFinallyStmt} -> ObjCAtFinallyStmt
  #{const CXCursor_ObjCAtThrowStmt} -> ObjCAtThrowStmt
  #{const CXCursor_ObjCAtSynchronizedStmt} -> ObjCAtSynchronizedStmt
  #{const CXCursor_ObjCAutoreleasePoolStmt} -> ObjCAutoreleasePoolStmt
  #{const CXCursor_ObjCForCollectionStmt} -> ObjCForCollectionStmt
  #{const CXCursor_CXXCatchStmt} -> CXXCatchStmt
  #{const CXCursor_CXXTryStmt} -> CXXTryStmt
  #{const CXCursor_CXXForRangeStmt} -> CXXForRangeStmt
  #{const CXCursor_SEHTryStmt} -> SEHTryStmt
  #{const CXCursor_SEHExceptStmt} -> SEHExceptStmt
  #{const CXCursor_SEHFinallyStmt} -> SEHFinallyStmt
  #{const CXCursor_MSAsmStmt} -> MSAsmStmt
  #{const CXCursor_NullStmt} -> NullStmt
  #{const CXCursor_DeclStmt} -> DeclStmt
  #{const CXCursor_OMPParallelDirective} -> OMPParallelDirective
  #{const CXCursor_OMPSimdDirective} -> OMPSimdDirective
  #{const CXCursor_OMPForDirective} -> OMPForDirective
  #{const CXCursor_OMPSectionsDirective} -> OMPSectionsDirective
  #{const CXCursor_OMPSectionDirective} -> OMPSectionDirective
  #{const CXCursor_OMPSingleDirective} -> OMPSingleDirective
  #{const CXCursor_OMPParallelForDirective} -> OMPParallelForDirective
  #{const CXCursor_OMPParallelSectionsDirective} -> OMPParallelSectionsDirective
  #{const CXCursor_OMPTaskDirective} -> OMPTaskDirective
  #{const CXCursor_OMPMasterDirective} -> OMPMasterDirective
  #{const CXCursor_OMPCriticalDirective} -> OMPCriticalDirective
  #{const CXCursor_OMPTaskyieldDirective} -> OMPTaskyieldDirective
  #{const CXCursor_OMPBarrierDirective} -> OMPBarrierDirective
  #{const CXCursor_OMPTaskwaitDirective} -> OMPTaskwaitDirective
  #{const CXCursor_OMPFlushDirective} -> OMPFlushDirective
  #{const CXCursor_SEHLeaveStmt} -> SEHLeaveStmt
  #{const CXCursor_LastStmt} -> LastStmt
  #{const CXCursor_TranslationUnit} -> TranslationUnit
  #{const CXCursor_FirstAttr} -> FirstAttr
  #{const CXCursor_UnexposedAttr} -> UnexposedAttr
  #{const CXCursor_IBActionAttr} -> IBActionAttr
  #{const CXCursor_IBOutletAttr} -> IBOutletAttr
  #{const CXCursor_IBOutletCollectionAttr} -> IBOutletCollectionAttr
  #{const CXCursor_CXXFinalAttr} -> CXXFinalAttr
  #{const CXCursor_CXXOverrideAttr} -> CXXOverrideAttr
  #{const CXCursor_AnnotateAttr} -> AnnotateAttr
  #{const CXCursor_AsmLabelAttr} -> AsmLabelAttr
  #{const CXCursor_PackedAttr} -> PackedAttr
  #{const CXCursor_PureAttr} -> PureAttr
  #{const CXCursor_ConstAttr} -> ConstAttr
  #{const CXCursor_NoDuplicateAttr} -> NoDuplicateAttr
  #{const CXCursor_CUDAConstantAttr} -> CUDAConstantAttr
  #{const CXCursor_CUDADeviceAttr} -> CUDADeviceAttr
  #{const CXCursor_CUDAGlobalAttr} -> CUDAGlobalAttr
  #{const CXCursor_CUDAHostAttr} -> CUDAHostAttr
  #{const CXCursor_LastAttr} -> LastAttr
  #{const CXCursor_PreprocessingDirective} -> PreprocessingDirective
  #{const CXCursor_MacroDefinition} -> MacroDefinition
  #{const CXCursor_MacroExpansion} -> MacroExpansion
  #{const CXCursor_MacroInstantiation} -> MacroInstantiation
  #{const CXCursor_InclusionDirective} -> InclusionDirective
  #{const CXCursor_FirstPreprocessing} -> FirstPreprocessing
  #{const CXCursor_LastPreprocessing} -> LastPreprocessing
  #{const CXCursor_ModuleImportDecl} -> ModuleImportDecl
  #{const CXCursor_FirstExtraDecl} -> FirstExtraDecl
  #{const CXCursor_LastExtraDecl} -> LastExtraDecl
  _ -> UnexposedDecl -- unrecognized enum value

cursorType :: Cursor -> Maybe Type
cursorType c = uderef c $ \cp -> do
  tp <- [C.block| CXType* {
    CXType type = clang_getCursorType(*$(CXCursor *cp));

    if (type.kind == CXType_Invalid) {
      return NULL;
    }

    return ALLOC(type);
    } |]
  if tp == nullPtr
    then return Nothing
    else (Just . Type) <$> newLeaf (parent c) (\_ -> return ( tp, free tp ))

typeArraySize :: Type -> Maybe Word64
typeArraySize t = uderef t $ \tp -> do
    as <- [C.exp| long long { clang_getArraySize(*$(CXType *tp)) } |]
    return $ if as == -1 then Nothing else Just (fromIntegral as)

typeCanonicalType :: Type -> Type
typeCanonicalType t = uderef t $ \tp -> do
  ctp <- [C.exp| CXType* { ALLOC(clang_getCanonicalType(*$(CXType *tp))) } |]
  Type <$> newLeaf (parent t) (\_ -> pure (ctp, free ctp))

typeElementType :: Type -> Maybe Type
typeElementType t = uderef t $ \tp -> do
  etp <- [C.block| CXType* {
    CXType type = clang_getElementType(*$(CXType *tp));

    if (type.kind == CXType_Invalid) {
      return NULL;
    }

    return ALLOC(type);
    } |]
  if etp == nullPtr
    then return Nothing
    else (Just . Type) <$> newLeaf (parent t) (\_ -> return ( etp, free etp ))

typePointeeType :: Type -> Maybe Type
typePointeeType t = uderef t $ \tp -> do
  etp <- [C.block| CXType* {
    CXType type = clang_getPointeeType(*$(CXType *tp));

    if (type.kind == CXType_Invalid) {
      return NULL;
    }

    return ALLOC(type);
    } |]
  if etp == nullPtr
    then return Nothing
    else (Just . Type) <$> newLeaf (parent t) (\_ -> return ( etp, free etp ))

typeResultType :: Type -> Maybe Type
typeResultType t = uderef t $ \tp -> do
  etp <- [C.block| CXType* {
    CXType type = clang_getResultType(*$(CXType *tp));

    if (type.kind == CXType_Invalid) {
      return NULL;
    }

    return ALLOC(type);
    } |]
  if etp == nullPtr
    then return Nothing
    else (Just . Type) <$> newLeaf (parent t) (\_ -> return ( etp, free etp ))

typeIsVariadic :: Type -> Bool
typeIsVariadic t = uderef t $ \tp -> do
  toBool <$> [C.exp| int {
    clang_isFunctionTypeVariadic(*$(CXType *tp))
    } |]

typeIsConstQualified :: Type -> Bool
typeIsConstQualified t = uderef t $ \tp -> do
  toBool <$> [C.exp| int {
    clang_isConstQualifiedType(*$(CXType *tp))
    } |]

typeNumArgTypes :: Type -> Int
typeNumArgTypes t = uderef t $ \tp -> do
  fromIntegral <$> [C.exp| int {
    clang_getNumArgTypes(*$(CXType *tp))
    } |]

typeArgType :: Type -> Int -> Maybe Type
typeArgType t i = uderef t $ \tp -> do
  let idx = fromIntegral i
  etp <- [C.block| CXType* {
    CXType type = clang_getArgType(*$(CXType *tp), $(int idx));
    if (type.kind == CXType_Invalid) {
      return NULL;
    }

    return ALLOC(type);
    } |]
  if etp == nullPtr
    then return Nothing
    else (Just . Type) <$> newLeaf (parent t) (\_ -> return ( etp, free etp ))

typeArgs :: Type -> Maybe [Type]
typeArgs t = sequence [typeArgType t i | i <- [0 .. typeNumArgTypes t - 1]]

cursorTypedefDeclUnderlyingType :: Cursor -> Maybe Type
cursorTypedefDeclUnderlyingType c = uderef c $ \cp -> do
  tp <- [C.block| CXType* {
    CXType type = clang_getTypedefDeclUnderlyingType(*$(CXCursor *cp));

    if (type.kind == CXType_Invalid) {
      return NULL;
    }

    return ALLOC(type);
    } |]
  if tp == nullPtr
    then return Nothing
    else (Just . Type) <$> newLeaf (parent c) (\_ -> return ( tp, free tp ))

typeDeclaration :: Type -> Maybe Cursor
typeDeclaration t = uderef t $ \tp -> do
  cp <- [C.block| CXCursor* {
    CXCursor ref = clang_getTypeDeclaration(*$(CXType *tp));

    if (clang_Cursor_isNull(ref)) {
      return NULL;
    }

    return ALLOC(ref);
    } |]
  if cp == nullPtr
    then return Nothing
    else (Just . Cursor) <$> newLeaf (parent t) (\_ -> return ( cp, free cp ))

typeKind :: Type -> TypeKind
typeKind t = uderef t $ fmap parseTypeKind . #peek CXType, kind

parseTypeKind :: CInt -> TypeKind
parseTypeKind = \case
  #{const CXType_Invalid} -> Invalid
  #{const CXType_Unexposed} -> Unexposed
  #{const CXType_Void} -> Void
  #{const CXType_Bool} -> Bool
  #{const CXType_Char_U} -> Char_U
  #{const CXType_UChar} -> UChar
  #{const CXType_Char16} -> Char16
  #{const CXType_Char32} -> Char32
  #{const CXType_UShort} -> UShort
  #{const CXType_UInt} -> UInt
  #{const CXType_ULong} -> ULong
  #{const CXType_ULongLong} -> ULongLong
  #{const CXType_UInt128} -> UInt128
  #{const CXType_Char_S} -> Char_S
  #{const CXType_SChar} -> SChar
  #{const CXType_WChar} -> WChar
  #{const CXType_Short} -> Short
  #{const CXType_Int} -> Int
  #{const CXType_Long} -> Long
  #{const CXType_LongLong} -> LongLong
  #{const CXType_Int128} -> Int128
  #{const CXType_Float} -> Float
  #{const CXType_Double} -> Double
  #{const CXType_LongDouble} -> LongDouble
  #{const CXType_NullPtr} -> NullPtr
  #{const CXType_Overload} -> Overload
  #{const CXType_Dependent} -> Dependent
  #{const CXType_ObjCId} -> ObjCId
  #{const CXType_ObjCClass} -> ObjCClass
  #{const CXType_ObjCSel} -> ObjCSel
  #{const CXType_FirstBuiltin} -> FirstBuiltin
  #{const CXType_LastBuiltin} -> LastBuiltin
  #{const CXType_Complex} -> Complex
  #{const CXType_Pointer} -> Pointer
  #{const CXType_BlockPointer} -> BlockPointer
  #{const CXType_LValueReference} -> LValueReference
  #{const CXType_RValueReference} -> RValueReference
  #{const CXType_Record} -> Record
  #{const CXType_Enum} -> Enum
  #{const CXType_Typedef} -> Typedef
  #{const CXType_ObjCInterface} -> ObjCInterface
  #{const CXType_ObjCObjectPointer} -> ObjCObjectPointer
  #{const CXType_FunctionNoProto} -> FunctionNoProto
  #{const CXType_FunctionProto} -> FunctionProto
  #{const CXType_ConstantArray} -> ConstantArray
  #{const CXType_Vector} -> Vector
  #{const CXType_IncompleteArray} -> IncompleteArray
  #{const CXType_VariableArray} -> VariableArray
  #{const CXType_DependentSizedArray} -> DependentSizedArray
  #{const CXType_MemberPointer} -> MemberPointer
  _ -> Unexposed

eitherTypeLayoutErrorOrWord64 :: CLLong -> Either TypeLayoutError Word64
eitherTypeLayoutErrorOrWord64 n = case n of
  #{const CXTypeLayoutError_Invalid} -> Left TypeLayoutErrorInvalid
  #{const CXTypeLayoutError_Incomplete} -> Left TypeLayoutErrorIncomplete
  #{const CXTypeLayoutError_Dependent} -> Left TypeLayoutErrorDependent
  _ -> Right $ fromIntegral n

typeSizeOf :: Type -> Either TypeLayoutError Word64
typeSizeOf t = uderef t $ \tp ->
  eitherTypeLayoutErrorOrWord64 <$>
  [C.exp| long long { clang_Type_getSizeOf(*$(CXType *tp)) } |]

offsetOfField :: Cursor -> Either TypeLayoutError Word64
offsetOfField c = uderef c $ \cp ->
  eitherTypeLayoutErrorOrWord64 <$>
  [C.exp| long long { clang_Cursor_getOffsetOfField(*$(CXCursor* cp)) } |]

typeSpelling :: Type -> ByteString
typeSpelling t = uderef t $ \tp ->
  withCXString $ \cxsp ->
    [C.exp| void { *$(CXString *cxsp) = clang_getTypeSpelling(*$(CXType *tp)); } |]

instance Clang Token where
  deref (Token ts i) f
    = deref (tokenSetRef ts) $ f . (`plusPtr` (i * (#size CXToken)))
  unsafeToPtr (Token ts i)
     = unsafeToPtr (tokenSetRef ts) `plusPtr` (i * (#size CXToken))
instance Child Token where
  parent (Token ts _) = parent (tokenSetRef ts)

foreign import ccall "clang_disposeTokens"
  clang_disposeTokens :: CXTranslationUnit -> Ptr CXToken -> CUInt -> Finalizer

tokenize :: SourceRange -> TokenSet
tokenize sr = unsafePerformIO $
  deref (parent sr) $ \tup ->
    deref sr $ \srp -> do
      ( tsp, tn ) <- C.withPtrs_ $ \( tspp, tnp ) ->
        [C.exp| void {
          clang_tokenize(
            $(CXTranslationUnit tup),
            *$(CXSourceRange *srp),
            $(CXToken **tspp),
            $(unsigned int *tnp));
          } |]
      tsn <- newLeaf (parent sr) $ \_ ->
        return ( tsp, clang_disposeTokens tup tsp tn )
      return $ TokenSet tsn (fromIntegral tn)

tokenSetTokens :: TokenSet -> [ Token ]
tokenSetTokens ts
  = map (Token ts) [1..tokenSetSize ts]

indexTokenSet :: TokenSet -> Int -> Token
indexTokenSet ts i
  | 0 <= i && i < tokenSetSize ts = Token ts i
  | otherwise = error "Token index out of bounds."

tokenSpelling :: Token -> ByteString
tokenSpelling t = unsafePerformIO $
  deref (parent t) $ \tup ->
    deref t $ \tp ->
      withCXString $ \cxsp -> do
        [C.block| void {
          *$(CXString *cxsp) = clang_getTokenSpelling(
            $(CXTranslationUnit tup),
            *$(CXToken *tp));
          } |]

isInSystemHeader :: SourceLocation -> Bool
isInSystemHeader l = uderef l $ \lp ->
  toBool <$> [C.exp| int {
    clang_Location_isInSystemHeader(*$(CXSourceLocation *lp))
    } |]

isFromMainFile :: SourceLocation -> Bool
isFromMainFile l = uderef l $ \lp ->
  toBool <$> [C.exp| int {
    clang_Location_isFromMainFile(*$(CXSourceLocation *lp))
    } |]

instance Show Cursor where
  show c =
    "Cursor { cursorKind = "
    ++ show (cursorKind c)
    ++ ", cursorSpelling = "
    ++ show (cursorSpelling c)
    ++ "}"

instance Show Type where
  show t =
    "Type { typeKind = "
    ++ show (typeKind t)
    ++ ", typeSpelling = "
    ++ show (typeSpelling t)
    ++ "}"

instance Show File where
  show f =
    "File { fileName = "
    ++ show (fileName f)
    ++ "}"
