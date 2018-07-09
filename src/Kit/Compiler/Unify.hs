module Kit.Compiler.Unify where

  import Control.Exception
  import Control.Monad
  import Kit.Ast
  import Kit.Compiler.Context
  import Kit.Error
  import Kit.HashTable
  import Kit.Parser.Span
  import Kit.Str

  checkConstraint :: TypeConstraint -> TypeInformation
  checkConstraint (TypeEq a b) = unify a b
  checkConstraint (TypeClassMember cls (TypeVar v)) = TypeVarHasConstraint v (TypeClassMember cls)
  checkConstraint (TypeClassMember TypeNotVoid (TypeBasicType BasicTypeVoid)) = TypeConstraintNotSatisfied
  checkConstraint (TypeClassMember TypeNotVoid t) = TypeConstraintSatisfied
  -- TODO
  checkConstraint (TypeClassMember TypeNumeric t) = TypeConstraintNotSatisfied
  checkConstraint (TypeClassMember TypeIntegral t) = TypeConstraintNotSatisfied
  checkConstraint (TypeClassMember TypeFloating t) = TypeConstraintNotSatisfied
  checkConstraint (TypeClassMember TypeString t) = TypeConstraintNotSatisfied
  checkConstraint (TypeClassMember TypeSequence t) = TypeConstraintNotSatisfied
  checkConstraint (TypeClassMember TypeIterable t) = TypeConstraintNotSatisfied

  -- Check whether type a unifies with b; i.e., can a value of type A be
  -- assigned to a variable of type B?
  unify :: ConcreteType -> ConcreteType -> TypeInformation
  unify x (TypeVar v) = TypeVarIs v x
  unify (TypeVar v) x = TypeVarIs v x
  unifyConcrete (TypeBasicType a) (TypeBasicType b) = unifyBasic a b
  unifyConcrete a b = if a == b then TypeConstraintSatisfied else TypeConstraintNotSatisfied

  unifyBasic :: BasicType -> BasicType -> TypeInformation
  unifyBasic (BasicTypeInt _) (BasicTypeInt _) = TypeConstraintSatisfied
  unifyBasic (BasicTypeUint _) (BasicTypeInt _) = TypeConstraintSatisfied
  unifyBasic (BasicTypeInt _) (BasicTypeUint _) = TypeConstraintSatisfied
  unifyBasic (BasicTypeUint _) (BasicTypeUint _) = TypeConstraintSatisfied
  unifyBasic (BasicTypeInt _) (BasicTypeFloat _) = TypeConstraintSatisfied
  unifyBasic (BasicTypeUint _) (BasicTypeFloat _) = TypeConstraintSatisfied
  unifyBasic (BasicTypeUnknown) (_) = TypeConstraintNotSatisfied
  unifyBasic a b = if a == b then TypeConstraintSatisfied else TypeConstraintNotSatisfied

  applySubstitution :: CompileContext -> TypeVar -> ConcreteType -> IO ()
  applySubstitution ctx tv ct = do
    existing <- getTypeVar ctx tv
    case existing of
      Just (Left constraints) -> do
        -- TODO: make sure we can resolve all constraints
        return ()
      _ -> return ()
    h_insert (ctxTypeVariables ctx) tv (Right ct)
    return ()

  resolveConstraint :: CompileContext -> Span -> TypeConstraint -> IO ()
  resolveConstraint ctx pos constraint = do
    result <- resolveConstraintOrThrow pos constraint
    case result of
      TypeVarIs id x -> applySubstitution ctx id x
      TypeVarHasConstraint id x -> return () -- TODO

  resolveConstraintOrThrow :: Span -> TypeConstraint -> IO TypeInformation
  resolveConstraintOrThrow pos t = do
    case checkConstraint t of
      TypeConstraintNotSatisfied -> constraintError t pos
      x -> return $ x

  resolveConstraints :: CompileContext -> Span -> [TypeConstraint] -> IO ()
  resolveConstraints ctx pos constraints = forM_ constraints (resolveConstraint ctx pos)

  constraintError (TypeEq a b) pos = do throw $ Errs [errp UnificationError ("Couldn't unify type constraints: " ++ (show a) ++ " and " ++ (show b)) (Just pos)]
  constraintError t pos = do throw $ Errs [errp UnificationError ("Couldn't satisfy type constraint: " ++ show t) (Just pos)]
