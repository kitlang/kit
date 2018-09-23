module Kit.Ir.IrDecl where

import Kit.Ast
import Kit.Ir.IrExpr
import Kit.Parser.Span
import Kit.Str

type IrDecl = Declaration IrExpr BasicType

data DeclBundle = DeclBundle TypePath [IrDecl]

bundleTp (DeclBundle x _) = x
bundleMembers (DeclBundle _ x) = x

mergeBundles x y = DeclBundle (bundleTp x) (bundleMembers x ++ bundleMembers y)
