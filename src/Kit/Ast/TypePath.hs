module Kit.Ast.TypePath where

import Kit.Ast.ModulePath
import Kit.Str

-- FIXME: there's some bending over backwards to ise these lists in the
-- "wrong" direction; would be better to reverse the namespace list

-- (Optional module path or empty, type name)
type TypePath = (ModulePath, Str)
showTypePath ([], s) = s
showTypePath (mp, s) = s_concat [showModulePath mp, ".", s]

tpName (_, s) = s
tpNamespace (n, _) = n
subPath (tp, n) s = (tp ++ [n], s)

addNamespace :: ModulePath -> TypePath -> TypePath
addNamespace n (_, s) = (n, s)

tpShift ([], s) = ([], s)
tpShift (n , s) = let (h : t) = reverse n in (reverse t, h)

modulePathToTypePath :: ModulePath -> TypePath
modulePathToTypePath mod = let (h : t) = reverse mod in (reverse t, h)

tpExtend (m, s) s2 = (m, s_concat [s, s2])
