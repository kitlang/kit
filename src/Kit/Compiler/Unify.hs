module Kit.Compiler.Unify where

  import Kit.Ast
  import Kit.Str

  type Substitution = (TypeSpec, TypeSpec)

  -- TODO
  -- Check whether type a unifies with b; i.e., can a value of type A be
  -- assigned to a variable of type B?
  unify :: TypeSpec -> TypeSpec -> (Bool, [Substitution])
  unify (TypeVar a) b = (True, [((TypeVar a), b)])
  unify a (TypeVar b) = (True, [((TypeVar b), a)])
  unify (ConcreteType a) (ConcreteType b) = unifyConcrete a b

  unifyConcrete :: ConcreteType -> ConcreteType -> (Bool, [Substitution])
  unifyConcrete (TypeBasicType a) (TypeBasicType b) = unifyBasic a b
  unifyConcrete a b = if a == b then (True, []) else (False, [])

  unifyBasic :: BasicType -> BasicType -> (Bool, [Substitution])
  unifyBasic (BasicTypeInt _) (BasicTypeInt _) = (True, [])
  unifyBasic (BasicTypeUint _) (BasicTypeInt _) = (True, [])
  unifyBasic (BasicTypeInt _) (BasicTypeUint _) = (True, [])
  unifyBasic (BasicTypeUint _) (BasicTypeUint _) = (True, [])
  unifyBasic (BasicTypeInt _) (BasicTypeFloat _) = (True, [])
  unifyBasic (BasicTypeUint _) (BasicTypeFloat _) = (True, [])
  unifyBasic (BasicTypeUnknown) (_) = (False, [])
  unifyBasic a b = if a == b then (True, []) else (False, [])
