module Kit.Ir.IrBundle where

import Kit.Ast
import Kit.Ir.IrStmt

data IrBundle = IrBundle TypePath [IrStmt]

bundleTp (IrBundle x _) = x
bundleMembers (IrBundle _ x) = x

mergeBundles x y = IrBundle (bundleTp x) (bundleMembers x ++ bundleMembers y)
