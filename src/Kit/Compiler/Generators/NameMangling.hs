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

validName :: CompileContext -> Str -> Str
validName ctx name = if ((ctxNameMangling ctx) && (s_length name > 31))
  then s_concat ["kit", s_hash name]
  else name

mangleName :: CompileContext -> [Str] -> Str -> Str
mangleName ctx [] s = s
mangleName ctx namespace s =
  validName ctx $ s_concat [s_join "_" ("kit" : namespace), "__", s]

monomorphName :: CompileContext -> Str -> [ConcreteType] -> Str
monomorphName ctx name p = if null p
  then name
  else if ctxNameMangling ctx
    then s_concat [name, "__", monomorphSuffix p]
    else s_join "__" (name : (map (s_pack . show) p))

monomorphSuffix :: [ConcreteType] -> Str
monomorphSuffix p = s_hash $ s_concat (map (s_pack . show) p)

enumDiscriminantName
  :: CompileContext -> TypePath -> [ConcreteType] -> Str -> IO Str
enumDiscriminantName ctx (modPath, name) p d = do
  defMod  <- getMod ctx modPath
  modTctx <- modTypeContext ctx defMod
  params  <- forM p (mapType $ follow ctx modTctx)
  let typeName = if null params then name else monomorphName ctx name params
  if modIsCModule defMod
    then return d
    else return $ mangleName ctx (modPath ++ [typeName]) d

discriminantMemberName :: Str -> Str
discriminantMemberName discriminant = s_concat ["variant_", discriminant]
