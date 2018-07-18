module Kit.Ast.TypeConstraint where

import Kit.Ast.BasicType
import Kit.Ast.ConcreteType
import Kit.Parser.Span

type UnresolvedTypeConstraint = ConcreteType -> TypeConstraint

data TypeConstraint
  = TypeEq ConcreteType ConcreteType String Span
  deriving (Eq, Show)

data TypeInformation
  = TypeVarIs TypeVar ConcreteType
  | TypeConstraintSatisfied
  | TypeConstraintNotSatisfied

instance Eq TypeInformation where
  (==) (TypeVarIs a b) (TypeVarIs c d) = (a == c) && (b == d)
  (==) TypeConstraintSatisfied TypeConstraintSatisfied = True
  (==) TypeConstraintNotSatisfied TypeConstraintNotSatisfied = True
  (==) a b = False

instance Show TypeInformation where
  show (TypeVarIs a b) = (show a) ++ " := " ++ (show b)
  show TypeConstraintSatisfied = "satisfied"
  show TypeConstraintNotSatisfied = "not satisfied"

data TypeVarInfo = TypeVarInfo
  { typeVarValue :: Maybe ConcreteType
  , typeVarPositions :: [Span]
  }

newTypeVarInfo p = TypeVarInfo
  { typeVarValue       = Nothing
  , typeVarPositions   = [p]
  }
addTypeVarPosition info p =
  info { typeVarPositions = p : typeVarPositions info }

typeClassNumeric = TypeConstrained [((["kit", "numeric"], "Numeric"), [])]
typeClassIntegral = TypeConstrained [((["kit", "numeric"], "Integral"), [])]
typeClassNumericMixed = TypeConstrained [((["kit", "numeric"], "NumericMixed"), [])]
typeClassIterable v = TypeConstrained [((["kit", "iterator"], "Iterable"), [])]
typeClassStringy = TypeConstrained [((["kit", "string"], "Stringy"), [])]
