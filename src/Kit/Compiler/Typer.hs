module Kit.Compiler.Typer where

  import Data.IORef
  import Kit.Ast
  import Kit.Str

  data TypeEnvironment = TypeEnvironment {
    last_type_var :: IORef TypeVar
  }

  newTypeEnvironment :: IO TypeEnvironment
  newTypeEnvironment = do
    ltv <- newIORef 0
    return $ TypeEnvironment {
      last_type_var = ltv
    }

  getTypeVar :: TypeEnvironment -> IO TypeVar
  getTypeVar e = do
    last <- readIORef (last_type_var e)
    let next = last + 1
    writeIORef (last_type_var e) next
    return $ next

  type Substitution = (TypeSpec, ConcreteType)

  -- TODO
  -- Given a type environment, infer the type of an expression
  infer :: TypeEnvironment -> Expr -> [Substitution]
  infer e x = []

  -- TODO
  -- Resolve a type specifier, following any typedefs
  follow :: TypeEnvironment -> TypeSpec -> [TypeParam] -> TypeSpec
  follow e t p = t

  {-
    Like follow, but also resolves abstracts, yielding the underlying
    storage type.
  -}
  concreteType :: TypeEnvironment -> TypeSpec -> ConcreteType
  -- TODO
  concreteType e t = TypeAtom "abc"
