module Kit.Ast.TypePath where

import Kit.Ast.ModulePath
import Kit.Str

-- (Optional module path or empty, type name)
type TypePath = (ModulePath, Str)
showTypePath ([], s) = s
showTypePath (mp, s) = s_concat [showModulePath mp, ".", s]
