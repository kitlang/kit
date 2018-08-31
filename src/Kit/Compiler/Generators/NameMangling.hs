module Kit.Compiler.Generators.NameMangling where

import Control.Exception
import Control.Monad
import Data.IORef
import Data.List
import Data.Maybe
import Kit.Ast
import Kit.Compiler.Binding
import Kit.Compiler.Module
import Kit.Compiler.Scope
import Kit.Compiler.TypeContext
import Kit.Compiler.TypedDecl
import Kit.Compiler.TypedExpr
import Kit.Compiler.Unify
import Kit.Compiler.Utils
import Kit.Error
import Kit.HashTable
import Kit.Ir
import Kit.Parser
import Kit.Str

validName :: Str -> Str
validName name =
  if s_length name > 63 then s_concat ["kit", s_hash name] else name

mangleName :: TypePath -> Str
mangleName ([], s) = s
mangleName (namespace, s) =
  validName $ s_concat [s_join "_" namespace, "__", s]

monomorphName :: TypePath -> [ConcreteType] -> TypePath
monomorphName tp [] = tp
monomorphName tp@(namespace, s) p =
  if null p then tp else (namespace, s_concat [s, "__", monomorphSuffix p])

tupleName :: [BasicType] -> Str
tupleName slots = s_concat ["tuple", hashParams slots]

monomorphSuffix :: [ConcreteType] -> Str
monomorphSuffix p = hashParams p

discriminantMemberName :: TypePath -> Str
discriminantMemberName discriminant =
  s_concat ["variant_", tpName discriminant]

hashParams :: (Show a) => [a] -> Str
hashParams p = s_hash $ s_concat $ map (s_pack . show) p
