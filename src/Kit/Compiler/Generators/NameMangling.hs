module Kit.Compiler.Generators.NameMangling where

import Control.Exception
import Control.Monad
import Data.IORef
import Data.List
import Data.Maybe
import Kit.Ast
import Kit.Compiler.Binding
import Kit.Compiler.Context
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
  if s_length name > 32 then s_concat ["kit", s_hash name] else name

mangleName :: [Str] -> Str -> Str
mangleName [] s = s
mangleName namespace s =
  validName $ s_concat [s_join "_" ("kit" : namespace), "__", s]

monomorphName :: Str -> [ConcreteType] -> Str
monomorphName name p =
  if null p then name else s_concat [name, "__", monomorphSuffix p]

monomorphSuffix :: [ConcreteType] -> Str
monomorphSuffix p = s_hash $ s_concat (map (s_pack . show) p)

enumDiscriminantName :: CompileContext -> TypePath -> [ConcreteType] -> Str -> IO Str
enumDiscriminantName ctx (modPath, name) p d = do
  defMod  <- getMod ctx modPath
  modTctx <- modTypeContext ctx defMod
  params  <- forM p (mapType $ follow ctx modTctx)
  let typeName = if null params then name else monomorphName name params
  if modIsCModule defMod
    then return d
    else return $ mangleName (modPath ++ [typeName]) d

discriminantMemberName :: Str -> Str
discriminantMemberName discriminant = s_concat ["variant_", discriminant]
