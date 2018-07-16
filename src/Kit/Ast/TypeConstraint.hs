module Kit.Ast.TypeConstraint where

import Kit.Ast.BasicType
import Kit.Ast.ConcreteType
import Kit.Parser.Span

type UnresolvedTypeConstraint = ConcreteType -> TypeConstraint

data TypeConstraint
  = TypeEq ConcreteType ConcreteType
  | TypeClassMember InternalTypeClass ConcreteType
  deriving (Eq, Show)

data InternalTypeClass
  = TypeNumeric
  | TypeIntegral
  | TypeNumericMixed
  | TypeString
  | TypeSequence ConcreteType
  | TypeIterable ConcreteType
  deriving (Eq)

instance Show InternalTypeClass where
  show TypeNumeric = "(numeric)"
  show TypeIntegral = "(integral)"
  show TypeNumericMixed = "(numeric, mixed)"
  show TypeString = "(string)"
  show (TypeSequence t) = "(sequence of " ++ (show t) ++ ")"
  show (TypeIterable t) = "(iterable of " ++ (show t) ++ ")"

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

data TypeVarInfo = TypeVarInfo
  { typeVarValue :: Maybe ConcreteType
  , typeVarPositions :: [Span]
  , typeVarConstraints :: [ConcreteType -> TypeConstraint]
  }

newTypeVarInfo p = TypeVarInfo
  { typeVarValue       = Nothing
  , typeVarPositions   = [p]
  , typeVarConstraints = []
  }
addTypeVarConstraint info c =
  info { typeVarConstraints = c : typeVarConstraints info }
addTypeVarPosition info p =
  info { typeVarPositions = p : typeVarPositions info }

{-
  For ergonomics, we'll use a representative default type for some internal
  typeclasses when no specific member is known.

  FIXME: these should be defined in user space ideally.
-}
defaultClassMember :: InternalTypeClass -> Maybe ConcreteType
defaultClassMember TypeNumeric      = Just $ TypeBasicType (BasicTypeInt 32)
defaultClassMember TypeIntegral     = Just $ TypeBasicType (BasicTypeInt 32)
defaultClassMember TypeNumericMixed = Just $ TypeBasicType (BasicTypeFloat 64)
defaultClassMember TypeString = Just $ TypeBasicType (CPtr (BasicTypeInt 8))
defaultClassMember (TypeSequence t) = Just $ TypeArr t Nothing
defaultClassMember (TypeIterable t) = Just $ TypeArr t Nothing
