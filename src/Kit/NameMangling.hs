module Kit.NameMangling where

import Data.List
import Data.Maybe
import Kit.Ast.BasicType
import Kit.Ast.ConcreteTypeBase
import Kit.Ast.TypePath
import Kit.Str

validName :: Str -> Str
validName name =
  if s_length name > 63 then s_concat ["kit", s_hash name] else name

mangleName :: TypePath -> Str
mangleName ([], s) = s
mangleName (namespace, s) =
  validName $ s_concat [s_join "_" namespace, "__", s]

monomorphName :: (Show a) => TypePath -> [ConcreteTypeBase a] -> TypePath
monomorphName tp [] = tp
monomorphName tp@(namespace, s) p =
  if null p then tp else monomorphAddSuffix tp $ Just $ monomorphSuffix p

tupleName :: [BasicType] -> Str
tupleName slots = s_concat ["tuple", hashParams slots]

monomorphSuffix :: (Show a) => [ConcreteTypeBase a] -> Str
monomorphSuffix p = hashParams p

monomorphAddSuffix :: TypePath -> Maybe Str -> TypePath
monomorphAddSuffix tp (Just suffix) = subPath tp suffix
monomorphAddSuffix tp Nothing       = tp

discriminantMemberName :: TypePath -> Str
discriminantMemberName discriminant =
  s_concat ["variant_", tpName discriminant]

hashParams :: (Show a) => [a] -> Str
hashParams p = s_hash $ s_concat $ map (s_pack . show) p
