module Kit.Ir.IrBundle where

import Kit.Ast
import Kit.Ir.IrDecl

data IrBundle = IrBundle TypePath [IrDecl]

bundleTp (IrBundle x _) = x
bundleMembers (IrBundle _ x) = x

mergeBundles x y = IrBundle (bundleTp x) (bundleMembers x ++ bundleMembers y)
