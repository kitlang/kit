module Kit.Ir.IrDecl where

import Kit.Ast
import Kit.Ir.IrExpr
import Kit.Parser.Span
import Kit.Str

type IrDecl = Declaration IrExpr BasicType

data DeclBundle = DeclBundle TypePath [IrDecl] [IncludeDependency]

bundleTp (DeclBundle x _ _) = x
bundleMembers (DeclBundle _ x _) = x
bundleDependencies (DeclBundle _ _ x) = x

mergeBundles x y = DeclBundle (bundleTp x)
                              (bundleMembers x ++ bundleMembers y)
                              (bundleDependencies x ++ bundleDependencies y)

data IncludeDependency
  = DeclDependency BasicType
  | DefDependency TypePath
  deriving (Eq, Show)

exprDeps :: IrExpr -> [IncludeDependency]
exprDeps irExpr = case irExpr of
  IrBlock      x  -> foldr (++) [] $ map exprDeps x
  IrCompound   x  -> foldr (++) [] $ map exprDeps x
  IrIdentifier tp -> [DefDependency tp]
  IrPreUnop  _ x  -> exprDeps x
  IrPostUnop _ x  -> exprDeps x
  IrBinop _ x y   -> exprDeps x ++ exprDeps y
  IrFor _ t x y z ->
    (typeDeps True t) ++ (exprDeps x ++ exprDeps y ++ exprDeps z)
  IrWhile x y _                 -> exprDeps x ++ exprDeps y
  IrIf    x y (Just z)          -> exprDeps x ++ exprDeps y ++ exprDeps z
  IrIf    x y Nothing           -> exprDeps x ++ exprDeps y
  IrReturn (Just x)             -> exprDeps x
  IrField       x _             -> exprDeps x
  IrArrayAccess x y             -> exprDeps x ++ exprDeps y
  IrCall x args -> exprDeps x ++ (foldr (++) [] $ map exprDeps args)
  IrCast        x t             -> (typeDeps True t) ++ (exprDeps x)
  IrCArrLiteral x               -> (foldr (++) [] $ map exprDeps x)
  IrVarDeclaration _ t (Just x) -> (typeDeps True t) ++ (exprDeps x)
  IrVarDeclaration _ t Nothing  -> typeDeps True t
  IrStructInit t fields ->
    (typeDeps True t) ++ (foldr (++) [] $ map (exprDeps . snd) fields)
  IrEnumInit t _ fields ->
    (typeDeps True t) ++ (foldr (++) [] $ map (exprDeps . snd) fields)
  IrTupleInit t fields ->
    (typeDeps True t) ++ (foldr (++) [] $ map exprDeps fields)
  IrSizeOf t -> typeDeps True t
  IrSwitch x cases (Just y) ->
    (exprDeps x) ++ (foldr (++) [] $ map (exprDeps . snd) cases) ++ exprDeps y
  IrSwitch x cases Nothing ->
    (exprDeps x) ++ (foldr (++) [] $ map (exprDeps . snd) cases)
  _ -> []

typeDeps :: Bool -> BasicType -> [IncludeDependency]
typeDeps def t =
  let softDep tp = if def then [DefDependency tp] else [DeclDependency t]
  in
    case t of
      CArray t _              -> typeDeps def t
      CPtr                 t  -> typeDeps def t
      BasicTypeStruct      tp -> softDep tp
      BasicTypeUnion       tp -> softDep tp
      BasicTypeSimpleEnum  tp -> softDep tp
      BasicTypeComplexEnum tp -> softDep tp
      BasicTypeFunction t args _ ->
        (typeDeps True t) ++ (foldr (++) [] $ map ((typeDeps True) . snd) args)
      BasicTypeTuple s _ -> softDep ([], s)
      _                  -> []
