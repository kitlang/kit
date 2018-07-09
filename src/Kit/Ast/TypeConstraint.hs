module Kit.Ast.TypeConstraint where

  import Kit.Ast.ConcreteType

  data TypeConstraint
    = TypeEq ConcreteType ConcreteType
    | TypeClassMember InternalTypeClass ConcreteType
    deriving (Eq, Show)

  data InternalTypeClass
    = TypeNumeric
    | TypeIntegral
    | TypeFloating
    | TypeString
    | TypeSequence
    | TypeIterable
    | TypeNotVoid
    deriving (Eq)

  instance Show InternalTypeClass where
    show TypeNumeric = "(numeric)"
    show TypeIntegral = "(integral)"
    show TypeFloating = "(floating)"
    show TypeString = "(string)"
    show TypeSequence = "(sequence)"
    show TypeIterable = "(iterable)"
    show TypeIterable = "(not void)"

  data TypeInformation
    = TypeVarIs TypeVar ConcreteType
    | TypeVarHasConstraint TypeVar (ConcreteType -> TypeConstraint)
    | TypeConstraintSatisfied
    | TypeConstraintNotSatisfied

  instance Eq TypeInformation where
    (==) (TypeVarIs a b) (TypeVarIs c d) = (a == c) && (b == d)
    (==) (TypeVarHasConstraint a b) (TypeVarHasConstraint c d) = (a == c) && ((b TypeRange) == (d TypeRange))
    (==) TypeConstraintSatisfied TypeConstraintSatisfied = True
    (==) TypeConstraintNotSatisfied TypeConstraintNotSatisfied = True
    (==) a b = False

  instance Show TypeInformation where
    show (TypeVarIs a b) = (show a) ++ " := " ++ (show b)
    show (TypeVarHasConstraint a b) = (show a) ++ " : "
    show TypeConstraintSatisfied = "satisfied"
    show TypeConstraintNotSatisfied = "not satisfied"

  type TypeVariableState = (Either [ConcreteType -> TypeConstraint] ConcreteType)
